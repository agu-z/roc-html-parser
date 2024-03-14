interface HtmlParser exposes [parse] imports []

Node : [
    Doctype Str,
    Element Str (List Attribute) (List Node),
    Comment Str,
    Text Str,
]

# TODO: Add locations

Attribute : (Str, Str)

State : {
    input : List U8,
    line : U32,
    column : U32,
}

parse : Str -> Result (List Node) _
parse = \input ->
    {
        input: Str.toUtf8 input,
        line: 1,
        column: 0,
    }
    |> ignoreSpaces
    |> parseNode # TODO: Parse many nodes
    |> Result.map \(node, _) -> [node]

parseNode : State -> Result (Node, State) _
parseNode = \state ->
    when symbol state '<' is
        Ok afterLt ->
            parseNonText afterLt

        Err _ ->
            (text, afterText) = chompWhile state \char -> char != '<'

            Ok (Text text, afterText)

parseNonText : State -> Result (Node, State) _
parseNonText = \afterLt ->
    (name, afterName) = chompWhile afterLt isName

    if Str.isEmpty name then
        parseCommentOrDocType afterLt
    else
        (attributes, afterAttributes) = zeroOrMore afterName parseAttribute

        afterGt <-
            ignoreSpaces afterAttributes
            |> symbol '>'
            |> Result.try

        # TODO: content
        (content, afterContent) = ([], afterGt)

        afterCloseSlash <- symbol2 afterContent '<' '/' |> Result.try
        (closingName, afterClosingName) = chompWhile afterCloseSlash isName
        afterCloseGt <- symbol afterClosingName '>' |> Result.try

        if name == closingName then
            Ok (Element name attributes content, afterCloseGt)
        else
            Err (MismatchedTag name closingName)

parseAttribute : State -> Result (Attribute, State) _
parseAttribute = \state ->
    (name, afterName) = chompWhile state isName

    if name == "" then
        Err (ExpectedAttributeName state)
    else
        afterSpaces1 = ignoreSpaces afterName

        when symbol afterSpaces1 '=' is
            Ok afterEqual ->
                afterSpaces2 = ignoreSpaces afterEqual

                when symbol afterSpaces2 '"' is
                    Ok afterQuote1 ->
                        (value, afterValue) = chompWhile afterQuote1 \char -> char != '"'

                        symbol afterValue '"'
                        |> Result.map \afterQuote2 ->
                            ((name, value), afterQuote2)

                    Err _ ->
                        (value, afterValue) = chompWhile afterSpaces2 \char -> char != ' ' && char != '>'

                        Ok ((name, value), afterValue)

            Err _ ->
                Ok ((name, ""), afterSpaces1)

parseCommentOrDocType : State -> Result (Node, State) _
parseCommentOrDocType = \afterLt ->
    afterExclamation <- symbol afterLt '!' |> Result.try

    when symbol2 afterExclamation '-' '-' is
        Ok afterStart ->
            parseComment afterStart

        Err _ ->
            parseDocType afterExclamation

parseComment : State -> Result (Node, State) _
parseComment = \afterOpen ->
    next = \state, acc ->
        when nextChar state is
            Next (char, newState) ->
                if char == '>' && List.endsWith acc ['-', '-'] then
                    acc
                    |> List.dropLast 2
                    |> Str.fromUtf8
                    |> Result.map \comment -> (Comment comment, newState)
                else
                    next newState (List.append acc char)

            End ->
                Err (EndedButExpected '-')

    next afterOpen (List.withCapacity 128)

parseDocType : State -> Result (Node, State) _
parseDocType = \_ ->
    crash "todo"

symbol : State, U8 -> Result State _
symbol = \state, expected ->
    when nextChar state is
        Next (char, rest) if char == expected ->
            Ok rest

        Next (char, _) ->
            Err (Expected expected char)

        End ->
            Err (EndedButExpected expected)

symbol2 : State, U8, U8 -> Result State _
symbol2 = \state, expected1, expected2 ->
    afterExpected1 <- symbol state expected1 |> Result.try
    symbol afterExpected1 expected2

zeroOrMore : State, (State -> Result (a, State) err) -> (List a, State)
zeroOrMore = \initState, parser ->
    next = \state, acc ->
        when parser (ignoreSpaces state) is
            Ok (value, newState) ->
                next (ignoreSpaces newState) (List.append acc value)

            Err _ ->
                (acc, state)

    # TODO: What is a good initial capacity?
    next initState (List.withCapacity 4)

ignoreSpaces : State -> State
ignoreSpaces = \state ->
    (_, after) = chompWhile state isBlank
    after

chompWhile : State, (U8 -> Bool) -> (Str, State)
chompWhile = \initState, predicate ->
    next = \state, acc ->
        when nextChar state is
            Next (char, newState) if predicate char ->
                next newState (List.append acc char)

            Next _ | End ->
                (
                    Result.withDefault (Str.fromUtf8 acc) "",
                    state,
                )

    next initState []

nextChar : State -> [Next (U8, State), End]
nextChar = \state ->
    when state.input is
        [] ->
            End

        [char, .. as rest] ->
            newState =
                if char == '\n' then
                    {
                        input: rest,
                        line: state.line + 1,
                        column: 1,
                    }
                else
                    {
                        input: rest,
                        line: state.line,
                        column: state.column + 1,
                    }

            Next (char, newState)

isName : U8 -> Bool
isName = \char -> isAsciiAlpha char || char == '-' || char == '_' || char == '.'

isAsciiAlpha : U8 -> Bool
isAsciiAlpha = \char -> (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')

isBlank : U8 -> Bool
isBlank = \char -> char == ' ' || char == '\t' || isNewLine char

isNewLine : U8 -> Bool
isNewLine = \char -> char == '\n'

# Tests


# Tags
expect parse "<p></p>" == Ok [Element "p" [] []]
expect parse "<p ></p>" == Ok [Element "p" [] []]

# Attributes
expect parse "<p id=\"name\"></p>" == Ok [Element "p" [("id", "name")] []]
expect parse "<p id = \"name\"  class= \"name\"></p>" == Ok [Element "p" [("id", "name"), ("class", "name")] []]
expect parse "<p id=name></p>" == Ok [Element "p" [("id", "name")] []]
expect parse "<button disabled></button>" == Ok [Element "button" [("disabled", "")] []]

# Mismatched tags
expect parse "<p></ul>" == Err (MismatchedTag "p" "ul")
expect parse "<input" == Err (EndedButExpected '>')
expect parse "<p>" == Err (EndedButExpected '<')

# Comments
expect parse "<!---->" == Ok [Comment ""]
expect parse "<!-- comment -->" == Ok [Comment " comment "]
expect parse "<!-- 8 > 5 -->" == Ok [Comment " 8 > 5 "]
expect parse "<!-- - - > -->" == Ok [Comment " - - > "]
expect parse "<!-- -- > -->" == Ok [Comment " -- > "]
expect parse "<!-- before -- after -->" == Ok [Comment " before -- after "]

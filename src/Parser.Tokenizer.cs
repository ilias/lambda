namespace LambdaCalculus;

// Split out tokenizer & related helpers for clarity.
internal sealed class Tokenizer
{
    private readonly Parser _parser;
    public Tokenizer(Parser parser) => _parser = parser;

    public List<Token> Tokenize(string input)
    {
        if (string.IsNullOrWhiteSpace(input)) return [];
        var result = new List<Token>();
        var term = new System.Text.StringBuilder();
        var pos = 0; int awaitingLambdaDot = 0;

        for (int i = 0; i < input.Length; i++)
        {
            var ch = input[i]; pos++;
            if (ch == '#') break; // comment rest of line

            // Whitespace acts as a delimiter between terms; ensure we flush accumulated term
            if (char.IsWhiteSpace(ch))
            {
                Flush();
                continue;
            }

            // unary minus -> integer
            if (ch == '-' && i + 1 < input.Length && char.IsDigit(input[i+1]))
            {
                bool isUnary = result.Count == 0 || result[^1].Type is TokenType.LParen or TokenType.LBracket or TokenType.Comma or TokenType.Semicolon or TokenType.Range or TokenType.FatArrow or TokenType.Arrow or TokenType.Equals;
                if (isUnary)
                {
                    int start = i; int startPos = pos; i++; pos++;
                    while (i < input.Length && char.IsDigit(input[i])) { i++; pos++; }
                    var num = input[start..i]; result.Add(new Token(TokenType.Integer, startPos, num)); i--; continue;
                }
            }

            var span = input.AsSpan(i);
            if (span.StartsWith("=>")) { Flush(); result.Add(new Token(TokenType.FatArrow, pos)); i++; pos++; continue; }
            if (span.StartsWith("->")) { Flush(); result.Add(new Token(TokenType.Arrow, pos)); i++; pos++; continue; }
            if (span.StartsWith("...")) { Flush(); result.Add(new Token(TokenType.Ellipsis, pos, "...")); i+=2; pos+=2; continue; }
            if (span.StartsWith("..")) { Flush(); result.Add(new Token(TokenType.Range, pos, "..")); i++; pos++; continue; }

            if (ch is '\\' or 'λ') { Flush(); result.Add(new Token(TokenType.Lambda, pos)); awaitingLambdaDot++; continue; }
            if (ch == '.' && awaitingLambdaDot > 0) { Flush(); result.Add(new Token(TokenType.Dot, pos)); awaitingLambdaDot--; continue; }

            // longest infix match
            if (!char.IsLetterOrDigit(ch) && !char.IsWhiteSpace(ch))
            {
                string? match = null;
                foreach (var op in _parser._infixOperators.Keys)
                    if (span.StartsWith(op) && (match == null || op.Length > match.Length)) match = op;
                if (match != null)
                {
                    Flush(); result.Add(new Token(TokenType.InfixOp, pos, match)); i += match.Length - 1; pos += match.Length - 1; continue; }
            }

            var token = ch switch
            {
                '(' => new Token(TokenType.LParen, pos),
                ')' => new Token(TokenType.RParen, pos),
                '[' => new Token(TokenType.LBracket, pos),
                ']' => new Token(TokenType.RBracket, pos),
                ',' => new Token(TokenType.Comma, pos),
                '$' => new Token(TokenType.Dollar, pos),
                ';' => new Token(TokenType.Semicolon, pos),
                '=' when term.Length == 0 && (i + 1 == input.Length || input[i+1] != '=') => new Token(TokenType.Equals, pos),
                char d when char.IsDigit(d) && term.Length == 0 => ParseInteger(input, ref i, ref pos),
                _ => null
            };

            if (ch == '=' && token == null && !char.IsWhiteSpace(ch)) { term.Append(ch); continue; }
            if (token == null)
            {
                term.Append(ch);
            }
            else
            {
                Flush(); result.Add(token);
            }
        }
        FlushFinal(); return result;

        void Flush()
        {
            if (term.Length == 0) return; var v = term.ToString(); var type = _parser.ClassifyTerm(v); result.Add(new Token(type, pos - term.Length, v)); term.Clear();
        }
        void FlushFinal()
        { if (term.Length > 0) { var v = term.ToString(); var type = _parser.ClassifyTerm(v); result.Add(new Token(type, pos - term.Length + 1, v)); } }
    }

    private static Token ParseInteger(string input, ref int i, ref int pos)
    { int start = i; int startPos = pos; while (i + 1 < input.Length && char.IsDigit(input[i+1])) { i++; pos++; } return new Token(TokenType.Integer, startPos, input[start..(i+1)]); }
}

const WS_CHARS =
    /[\f\n\r\t, \u000B\u001C\u001D\u001E\u001F\u2028\u2029\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2008\u2009\u200a\u205f\u3000]/;

const WS = token(repeat1(WS_CHARS));

const COMMENT = token(/(-|#!).*\n?/);

const STRING = token(seq('"', repeat(/[^"\\]/), '"'));

const UNKNOWN = token(repeat(/[^\n]/), '\n');

module.exports = grammar({
    name: "toylisp",
    rules: {
        source: ($) => repeat(choice($._ws, $.comment, $.string, $.unknown)),
        _ws: ($) => WS,
        comment: ($) => COMMENT,
        string: ($) => STRING,
        unknown: ($) => UNKNOWN,
    },
});

;;; eaw.el --- Fix east asian ambiguous width issue for emacs
;; Author: HAMANO Tsukasa <code@cuspy.org>
;; URL: https://github.com/hamano/locale-eaw
;; Version: 12
;; MIT License

;;; Code:

(setq east-asian-ambiguous '(
                             #x00A1 ; Po         INVERTED EXCLAMATION MARK
                             #x00A4 ; Sc         CURRENCY SIGN
                             #x00A7 ; Po         SECTION SIGN
                             #x00A8 ; Sk         DIAERESIS
                             #x00AA ; Lo         FEMININE ORDINAL INDICATOR
                             #x00AD ; Cf         SOFT HYPHEN
                             #x00AE ; So         REGISTERED SIGN
                             #x00B0 ; So         DEGREE SIGN
                             #x00B1 ; Sm         PLUS-MINUS SIGN
                             #x00B2 ; No     [2] SUPERSCRIPT TWO..SUPERSCRIPT THREE
                             #x00B3 ; No     [2] SUPERSCRIPT TWO..SUPERSCRIPT THREE
                             #x00B4 ; Sk         ACUTE ACCENT
                             #x00B6 ; Po     [2] PILCROW SIGN..MIDDLE DOT
                             #x00B7 ; Po     [2] PILCROW SIGN..MIDDLE DOT
                             #x00B8 ; Sk         CEDILLA
                             #x00B9 ; No         SUPERSCRIPT ONE
                             #x00BA ; Lo         MASCULINE ORDINAL INDICATOR
                             #x00BC ; No     [3] VULGAR FRACTION ONE QUARTER..VULGAR FRACTION THREE QUARTERS
                             #x00BD ; No     [3] VULGAR FRACTION ONE QUARTER..VULGAR FRACTION THREE QUARTERS
                             #x00BE ; No     [3] VULGAR FRACTION ONE QUARTER..VULGAR FRACTION THREE QUARTERS
                             #x00BF ; Po         INVERTED QUESTION MARK
                             #x00C6 ; Lu         LATIN CAPITAL LETTER AE
                             #x00D0 ; Lu         LATIN CAPITAL LETTER ETH
                             #x00D7 ; Sm         MULTIPLICATION SIGN
                             #x00D8 ; Lu         LATIN CAPITAL LETTER O WITH STROKE
                             #x00DE ; L&     [4] LATIN CAPITAL LETTER THORN..LATIN SMALL LETTER A WITH ACUTE
                             #x00DF ; L&     [4] LATIN CAPITAL LETTER THORN..LATIN SMALL LETTER A WITH ACUTE
                             #x00E0 ; L&     [4] LATIN CAPITAL LETTER THORN..LATIN SMALL LETTER A WITH ACUTE
                             #x00E1 ; L&     [4] LATIN CAPITAL LETTER THORN..LATIN SMALL LETTER A WITH ACUTE
                             #x00E6 ; Ll         LATIN SMALL LETTER AE
                             #x00E8 ; Ll     [3] LATIN SMALL LETTER E WITH GRAVE..LATIN SMALL LETTER E WITH CIRCUMFLEX
                             #x00E9 ; Ll     [3] LATIN SMALL LETTER E WITH GRAVE..LATIN SMALL LETTER E WITH CIRCUMFLEX
                             #x00EA ; Ll     [3] LATIN SMALL LETTER E WITH GRAVE..LATIN SMALL LETTER E WITH CIRCUMFLEX
                             #x00EC ; Ll     [2] LATIN SMALL LETTER I WITH GRAVE..LATIN SMALL LETTER I WITH ACUTE
                             #x00ED ; Ll     [2] LATIN SMALL LETTER I WITH GRAVE..LATIN SMALL LETTER I WITH ACUTE
                             #x00F0 ; Ll         LATIN SMALL LETTER ETH
                             #x00F2 ; Ll     [2] LATIN SMALL LETTER O WITH GRAVE..LATIN SMALL LETTER O WITH ACUTE
                             #x00F3 ; Ll     [2] LATIN SMALL LETTER O WITH GRAVE..LATIN SMALL LETTER O WITH ACUTE
                             #x00F7 ; Sm         DIVISION SIGN
                             #x00F8 ; Ll     [3] LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER U WITH ACUTE
                             #x00F9 ; Ll     [3] LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER U WITH ACUTE
                             #x00FA ; Ll     [3] LATIN SMALL LETTER O WITH STROKE..LATIN SMALL LETTER U WITH ACUTE
                             #x00FC ; Ll         LATIN SMALL LETTER U WITH DIAERESIS
                             #x00FE ; Ll         LATIN SMALL LETTER THORN
                             #x0101 ; Ll         LATIN SMALL LETTER A WITH MACRON
                             #x0111 ; Ll         LATIN SMALL LETTER D WITH STROKE
                             #x0113 ; Ll         LATIN SMALL LETTER E WITH MACRON
                             #x011B ; Ll         LATIN SMALL LETTER E WITH CARON
                             #x0126 ; L&     [2] LATIN CAPITAL LETTER H WITH STROKE..LATIN SMALL LETTER H WITH STROKE
                             #x0127 ; L&     [2] LATIN CAPITAL LETTER H WITH STROKE..LATIN SMALL LETTER H WITH STROKE
                             #x012B ; Ll         LATIN SMALL LETTER I WITH MACRON
                             #x0131 ; L&     [3] LATIN SMALL LETTER DOTLESS I..LATIN SMALL LIGATURE IJ
                             #x0132 ; L&     [3] LATIN SMALL LETTER DOTLESS I..LATIN SMALL LIGATURE IJ
                             #x0133 ; L&     [3] LATIN SMALL LETTER DOTLESS I..LATIN SMALL LIGATURE IJ
                             #x0138 ; Ll         LATIN SMALL LETTER KRA
                             #x013F ; L&     [4] LATIN CAPITAL LETTER L WITH MIDDLE DOT..LATIN SMALL LETTER L WITH STROKE
                             #x0140 ; L&     [4] LATIN CAPITAL LETTER L WITH MIDDLE DOT..LATIN SMALL LETTER L WITH STROKE
                             #x0141 ; L&     [4] LATIN CAPITAL LETTER L WITH MIDDLE DOT..LATIN SMALL LETTER L WITH STROKE
                             #x0142 ; L&     [4] LATIN CAPITAL LETTER L WITH MIDDLE DOT..LATIN SMALL LETTER L WITH STROKE
                             #x0144 ; Ll         LATIN SMALL LETTER N WITH ACUTE
                             #x0148 ; L&     [4] LATIN SMALL LETTER N WITH CARON..LATIN SMALL LETTER ENG
                             #x0149 ; L&     [4] LATIN SMALL LETTER N WITH CARON..LATIN SMALL LETTER ENG
                             #x014A ; L&     [4] LATIN SMALL LETTER N WITH CARON..LATIN SMALL LETTER ENG
                             #x014B ; L&     [4] LATIN SMALL LETTER N WITH CARON..LATIN SMALL LETTER ENG
                             #x014D ; Ll         LATIN SMALL LETTER O WITH MACRON
                             #x0152 ; L&     [2] LATIN CAPITAL LIGATURE OE..LATIN SMALL LIGATURE OE
                             #x0153 ; L&     [2] LATIN CAPITAL LIGATURE OE..LATIN SMALL LIGATURE OE
                             #x0166 ; L&     [2] LATIN CAPITAL LETTER T WITH STROKE..LATIN SMALL LETTER T WITH STROKE
                             #x0167 ; L&     [2] LATIN CAPITAL LETTER T WITH STROKE..LATIN SMALL LETTER T WITH STROKE
                             #x016B ; Ll         LATIN SMALL LETTER U WITH MACRON
                             #x01CE ; Ll         LATIN SMALL LETTER A WITH CARON
                             #x01D0 ; Ll         LATIN SMALL LETTER I WITH CARON
                             #x01D2 ; Ll         LATIN SMALL LETTER O WITH CARON
                             #x01D4 ; Ll         LATIN SMALL LETTER U WITH CARON
                             #x01D6 ; Ll         LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
                             #x01D8 ; Ll         LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE
                             #x01DA ; Ll         LATIN SMALL LETTER U WITH DIAERESIS AND CARON
                             #x01DC ; Ll         LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
                             #x0251 ; Ll         LATIN SMALL LETTER ALPHA
                             #x0261 ; Ll         LATIN SMALL LETTER SCRIPT G
                             #x02C4 ; Sk         MODIFIER LETTER UP ARROWHEAD
                             #x02C7 ; Lm         CARON
                             #x02C9 ; Lm     [3] MODIFIER LETTER MACRON..MODIFIER LETTER GRAVE ACCENT
                             #x02CA ; Lm     [3] MODIFIER LETTER MACRON..MODIFIER LETTER GRAVE ACCENT
                             #x02CB ; Lm     [3] MODIFIER LETTER MACRON..MODIFIER LETTER GRAVE ACCENT
                             #x02CD ; Lm         MODIFIER LETTER LOW MACRON
                             #x02D0 ; Lm         MODIFIER LETTER TRIANGULAR COLON
                             #x02D8 ; Sk     [4] BREVE..OGONEK
                             #x02D9 ; Sk     [4] BREVE..OGONEK
                             #x02DA ; Sk     [4] BREVE..OGONEK
                             #x02DB ; Sk     [4] BREVE..OGONEK
                             #x02DD ; Sk         DOUBLE ACUTE ACCENT
                             #x02DF ; Sk         MODIFIER LETTER CROSS ACCENT
                             #x0391 ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x0392 ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x0393 ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x0394 ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x0395 ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x0396 ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x0397 ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x0398 ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x0399 ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x039A ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x039B ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x039C ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x039D ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x039E ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x039F ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x03A0 ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x03A1 ; Lu    [17] GREEK CAPITAL LETTER ALPHA..GREEK CAPITAL LETTER RHO
                             #x03A3 ; Lu     [7] GREEK CAPITAL LETTER SIGMA..GREEK CAPITAL LETTER OMEGA
                             #x03A4 ; Lu     [7] GREEK CAPITAL LETTER SIGMA..GREEK CAPITAL LETTER OMEGA
                             #x03A5 ; Lu     [7] GREEK CAPITAL LETTER SIGMA..GREEK CAPITAL LETTER OMEGA
                             #x03A6 ; Lu     [7] GREEK CAPITAL LETTER SIGMA..GREEK CAPITAL LETTER OMEGA
                             #x03A7 ; Lu     [7] GREEK CAPITAL LETTER SIGMA..GREEK CAPITAL LETTER OMEGA
                             #x03A8 ; Lu     [7] GREEK CAPITAL LETTER SIGMA..GREEK CAPITAL LETTER OMEGA
                             #x03A9 ; Lu     [7] GREEK CAPITAL LETTER SIGMA..GREEK CAPITAL LETTER OMEGA
                             #x03B1 ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03B2 ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03B3 ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03B4 ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03B5 ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03B6 ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03B7 ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03B8 ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03B9 ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03BA ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03BB ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03BC ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03BD ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03BE ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03BF ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03C0 ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03C1 ; Ll    [17] GREEK SMALL LETTER ALPHA..GREEK SMALL LETTER RHO
                             #x03C3 ; Ll     [7] GREEK SMALL LETTER SIGMA..GREEK SMALL LETTER OMEGA
                             #x03C4 ; Ll     [7] GREEK SMALL LETTER SIGMA..GREEK SMALL LETTER OMEGA
                             #x03C5 ; Ll     [7] GREEK SMALL LETTER SIGMA..GREEK SMALL LETTER OMEGA
                             #x03C6 ; Ll     [7] GREEK SMALL LETTER SIGMA..GREEK SMALL LETTER OMEGA
                             #x03C7 ; Ll     [7] GREEK SMALL LETTER SIGMA..GREEK SMALL LETTER OMEGA
                             #x03C8 ; Ll     [7] GREEK SMALL LETTER SIGMA..GREEK SMALL LETTER OMEGA
                             #x03C9 ; Ll     [7] GREEK SMALL LETTER SIGMA..GREEK SMALL LETTER OMEGA
                             #x0401 ; Lu         CYRILLIC CAPITAL LETTER IO
                             #x0410 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0411 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0412 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0413 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0414 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0415 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0416 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0417 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0418 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0419 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x041A ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x041B ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x041C ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x041D ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x041E ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x041F ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0420 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0421 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0422 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0423 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0424 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0425 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0426 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0427 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0428 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0429 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x042A ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x042B ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x042C ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x042D ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x042E ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x042F ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0430 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0431 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0432 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0433 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0434 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0435 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0436 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0437 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0438 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0439 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x043A ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x043B ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x043C ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x043D ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x043E ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x043F ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0440 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0441 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0442 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0443 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0444 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0445 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0446 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0447 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0448 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0449 ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x044A ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x044B ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x044C ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x044D ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x044E ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x044F ; L&    [64] CYRILLIC CAPITAL LETTER A..CYRILLIC SMALL LETTER YA
                             #x0451 ; Ll         CYRILLIC SMALL LETTER IO
                             #x2010 ; Pd         HYPHEN
                             #x2013 ; Pd     [3] EN DASH..HORIZONTAL BAR
                             #x2014 ; Pd     [3] EN DASH..HORIZONTAL BAR
                             #x2015 ; Pd     [3] EN DASH..HORIZONTAL BAR
                             #x2016 ; Po         DOUBLE VERTICAL LINE
                             #x2018 ; Pi         LEFT SINGLE QUOTATION MARK
                             #x2019 ; Pf         RIGHT SINGLE QUOTATION MARK
                             #x201C ; Pi         LEFT DOUBLE QUOTATION MARK
                             #x201D ; Pf         RIGHT DOUBLE QUOTATION MARK
                             #x2020 ; Po     [3] DAGGER..BULLET
                             #x2021 ; Po     [3] DAGGER..BULLET
                             #x2022 ; Po     [3] DAGGER..BULLET
                             #x2024 ; Po     [4] ONE DOT LEADER..HYPHENATION POINT
                             #x2025 ; Po     [4] ONE DOT LEADER..HYPHENATION POINT
                             #x2026 ; Po     [4] ONE DOT LEADER..HYPHENATION POINT
                             #x2027 ; Po     [4] ONE DOT LEADER..HYPHENATION POINT
                             #x2030 ; Po         PER MILLE SIGN
                             #x2032 ; Po     [2] PRIME..DOUBLE PRIME
                             #x2033 ; Po     [2] PRIME..DOUBLE PRIME
                             #x2035 ; Po         REVERSED PRIME
                             #x203B ; Po         REFERENCE MARK
                             #x203E ; Po         OVERLINE
                             #x2074 ; No         SUPERSCRIPT FOUR
                             #x207F ; Lm         SUPERSCRIPT LATIN SMALL LETTER N
                             #x2081 ; No     [4] SUBSCRIPT ONE..SUBSCRIPT FOUR
                             #x2082 ; No     [4] SUBSCRIPT ONE..SUBSCRIPT FOUR
                             #x2083 ; No     [4] SUBSCRIPT ONE..SUBSCRIPT FOUR
                             #x2084 ; No     [4] SUBSCRIPT ONE..SUBSCRIPT FOUR
                             #x20AC ; Sc         EURO SIGN
                             #x2103 ; So         DEGREE CELSIUS
                             #x2105 ; So         CARE OF
                             #x2109 ; So         DEGREE FAHRENHEIT
                             #x2113 ; Ll         SCRIPT SMALL L
                             #x2116 ; So         NUMERO SIGN
                             #x2121 ; So     [2] TELEPHONE SIGN..TRADE MARK SIGN
                             #x2122 ; So     [2] TELEPHONE SIGN..TRADE MARK SIGN
                             #x2126 ; Lu         OHM SIGN
                             #x212B ; Lu         ANGSTROM SIGN
                             #x2153 ; No     [2] VULGAR FRACTION ONE THIRD..VULGAR FRACTION TWO THIRDS
                             #x2154 ; No     [2] VULGAR FRACTION ONE THIRD..VULGAR FRACTION TWO THIRDS
                             #x215B ; No     [4] VULGAR FRACTION ONE EIGHTH..VULGAR FRACTION SEVEN EIGHTHS
                             #x215C ; No     [4] VULGAR FRACTION ONE EIGHTH..VULGAR FRACTION SEVEN EIGHTHS
                             #x215D ; No     [4] VULGAR FRACTION ONE EIGHTH..VULGAR FRACTION SEVEN EIGHTHS
                             #x215E ; No     [4] VULGAR FRACTION ONE EIGHTH..VULGAR FRACTION SEVEN EIGHTHS
                             #x2160 ; Nl    [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
                             #x2161 ; Nl    [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
                             #x2162 ; Nl    [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
                             #x2163 ; Nl    [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
                             #x2164 ; Nl    [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
                             #x2165 ; Nl    [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
                             #x2166 ; Nl    [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
                             #x2167 ; Nl    [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
                             #x2168 ; Nl    [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
                             #x2169 ; Nl    [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
                             #x216A ; Nl    [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
                             #x216B ; Nl    [12] ROMAN NUMERAL ONE..ROMAN NUMERAL TWELVE
                             #x2170 ; Nl    [10] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL TEN
                             #x2171 ; Nl    [10] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL TEN
                             #x2172 ; Nl    [10] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL TEN
                             #x2173 ; Nl    [10] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL TEN
                             #x2174 ; Nl    [10] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL TEN
                             #x2175 ; Nl    [10] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL TEN
                             #x2176 ; Nl    [10] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL TEN
                             #x2177 ; Nl    [10] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL TEN
                             #x2178 ; Nl    [10] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL TEN
                             #x2179 ; Nl    [10] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL TEN
                             #x2189 ; No         VULGAR FRACTION ZERO THIRDS
                             #x2190 ; Sm     [5] LEFTWARDS ARROW..LEFT RIGHT ARROW
                             #x2191 ; Sm     [5] LEFTWARDS ARROW..LEFT RIGHT ARROW
                             #x2192 ; Sm     [5] LEFTWARDS ARROW..LEFT RIGHT ARROW
                             #x2193 ; Sm     [5] LEFTWARDS ARROW..LEFT RIGHT ARROW
                             #x2194 ; Sm     [5] LEFTWARDS ARROW..LEFT RIGHT ARROW
                             #x2195 ; So     [5] UP DOWN ARROW..SOUTH WEST ARROW
                             #x2196 ; So     [5] UP DOWN ARROW..SOUTH WEST ARROW
                             #x2197 ; So     [5] UP DOWN ARROW..SOUTH WEST ARROW
                             #x2198 ; So     [5] UP DOWN ARROW..SOUTH WEST ARROW
                             #x2199 ; So     [5] UP DOWN ARROW..SOUTH WEST ARROW
                             #x21B8 ; So     [2] NORTH WEST ARROW TO LONG BAR..LEFTWARDS ARROW TO BAR OVER RIGHTWARDS ARROW TO BAR
                             #x21B9 ; So     [2] NORTH WEST ARROW TO LONG BAR..LEFTWARDS ARROW TO BAR OVER RIGHTWARDS ARROW TO BAR
                             #x21D2 ; Sm         RIGHTWARDS DOUBLE ARROW
                             #x21D4 ; Sm         LEFT RIGHT DOUBLE ARROW
                             #x21E7 ; So         UPWARDS WHITE ARROW
                             #x2200 ; Sm         FOR ALL
                             #x2202 ; Sm     [2] PARTIAL DIFFERENTIAL..THERE EXISTS
                             #x2203 ; Sm     [2] PARTIAL DIFFERENTIAL..THERE EXISTS
                             #x2207 ; Sm     [2] NABLA..ELEMENT OF
                             #x2208 ; Sm     [2] NABLA..ELEMENT OF
                             #x220B ; Sm         CONTAINS AS MEMBER
                             #x220F ; Sm         N-ARY PRODUCT
                             #x2211 ; Sm         N-ARY SUMMATION
                             #x2215 ; Sm         DIVISION SLASH
                             #x221A ; Sm         SQUARE ROOT
                             #x221D ; Sm     [4] PROPORTIONAL TO..ANGLE
                             #x221E ; Sm     [4] PROPORTIONAL TO..ANGLE
                             #x221F ; Sm     [4] PROPORTIONAL TO..ANGLE
                             #x2220 ; Sm     [4] PROPORTIONAL TO..ANGLE
                             #x2223 ; Sm         DIVIDES
                             #x2225 ; Sm         PARALLEL TO
                             #x2227 ; Sm     [6] LOGICAL AND..DOUBLE INTEGRAL
                             #x2228 ; Sm     [6] LOGICAL AND..DOUBLE INTEGRAL
                             #x2229 ; Sm     [6] LOGICAL AND..DOUBLE INTEGRAL
                             #x222A ; Sm     [6] LOGICAL AND..DOUBLE INTEGRAL
                             #x222B ; Sm     [6] LOGICAL AND..DOUBLE INTEGRAL
                             #x222C ; Sm     [6] LOGICAL AND..DOUBLE INTEGRAL
                             #x222E ; Sm         CONTOUR INTEGRAL
                             #x2234 ; Sm     [4] THEREFORE..PROPORTION
                             #x2235 ; Sm     [4] THEREFORE..PROPORTION
                             #x2236 ; Sm     [4] THEREFORE..PROPORTION
                             #x2237 ; Sm     [4] THEREFORE..PROPORTION
                             #x223C ; Sm     [2] TILDE OPERATOR..REVERSED TILDE
                             #x223D ; Sm     [2] TILDE OPERATOR..REVERSED TILDE
                             #x2248 ; Sm         ALMOST EQUAL TO
                             #x224C ; Sm         ALL EQUAL TO
                             #x2252 ; Sm         APPROXIMATELY EQUAL TO OR THE IMAGE OF
                             #x2260 ; Sm     [2] NOT EQUAL TO..IDENTICAL TO
                             #x2261 ; Sm     [2] NOT EQUAL TO..IDENTICAL TO
                             #x2264 ; Sm     [4] LESS-THAN OR EQUAL TO..GREATER-THAN OVER EQUAL TO
                             #x2265 ; Sm     [4] LESS-THAN OR EQUAL TO..GREATER-THAN OVER EQUAL TO
                             #x2266 ; Sm     [4] LESS-THAN OR EQUAL TO..GREATER-THAN OVER EQUAL TO
                             #x2267 ; Sm     [4] LESS-THAN OR EQUAL TO..GREATER-THAN OVER EQUAL TO
                             #x226A ; Sm     [2] MUCH LESS-THAN..MUCH GREATER-THAN
                             #x226B ; Sm     [2] MUCH LESS-THAN..MUCH GREATER-THAN
                             #x226E ; Sm     [2] NOT LESS-THAN..NOT GREATER-THAN
                             #x226F ; Sm     [2] NOT LESS-THAN..NOT GREATER-THAN
                             #x2282 ; Sm     [2] SUBSET OF..SUPERSET OF
                             #x2283 ; Sm     [2] SUBSET OF..SUPERSET OF
                             #x2286 ; Sm     [2] SUBSET OF OR EQUAL TO..SUPERSET OF OR EQUAL TO
                             #x2287 ; Sm     [2] SUBSET OF OR EQUAL TO..SUPERSET OF OR EQUAL TO
                             #x2295 ; Sm         CIRCLED PLUS
                             #x2299 ; Sm         CIRCLED DOT OPERATOR
                             #x22A5 ; Sm         UP TACK
                             #x22BF ; Sm         RIGHT TRIANGLE
                             #x2312 ; So         ARC
                             #x2460 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2461 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2462 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2463 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2464 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2465 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2466 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2467 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2468 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2469 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x246A ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x246B ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x246C ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x246D ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x246E ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x246F ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2470 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2471 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2472 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2473 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2474 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2475 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2476 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2477 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2478 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2479 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x247A ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x247B ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x247C ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x247D ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x247E ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x247F ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2480 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2481 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2482 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2483 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2484 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2485 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2486 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2487 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2488 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2489 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x248A ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x248B ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x248C ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x248D ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x248E ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x248F ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2490 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2491 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2492 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2493 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2494 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2495 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2496 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2497 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2498 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x2499 ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x249A ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x249B ; No    [60] CIRCLED DIGIT ONE..NUMBER TWENTY FULL STOP
                             #x249C ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x249D ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x249E ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x249F ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24A0 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24A1 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24A2 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24A3 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24A4 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24A5 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24A6 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24A7 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24A8 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24A9 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24AA ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24AB ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24AC ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24AD ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24AE ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24AF ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24B0 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24B1 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24B2 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24B3 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24B4 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24B5 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24B6 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24B7 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24B8 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24B9 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24BA ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24BB ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24BC ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24BD ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24BE ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24BF ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24C0 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24C1 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24C2 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24C3 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24C4 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24C5 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24C6 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24C7 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24C8 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24C9 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24CA ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24CB ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24CC ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24CD ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24CE ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24CF ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24D0 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24D1 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24D2 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24D3 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24D4 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24D5 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24D6 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24D7 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24D8 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24D9 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24DA ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24DB ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24DC ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24DD ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24DE ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24DF ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24E0 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24E1 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24E2 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24E3 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24E4 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24E5 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24E6 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24E7 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24E8 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24E9 ; So    [78] PARENTHESIZED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
                             #x24EB ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24EC ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24ED ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24EE ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24EF ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24F0 ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24F1 ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24F2 ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24F3 ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24F4 ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24F5 ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24F6 ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24F7 ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24F8 ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24F9 ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24FA ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24FB ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24FC ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24FD ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24FE ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x24FF ; No    [21] NEGATIVE CIRCLED NUMBER ELEVEN..NEGATIVE CIRCLED DIGIT ZERO
                             #x2500 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2501 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2502 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2503 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2504 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2505 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2506 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2507 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2508 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2509 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x250A ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x250B ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x250C ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x250D ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x250E ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x250F ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2510 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2511 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2512 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2513 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2514 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2515 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2516 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2517 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2518 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2519 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x251A ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x251B ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x251C ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x251D ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x251E ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x251F ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2520 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2521 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2522 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2523 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2524 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2525 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2526 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2527 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2528 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2529 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x252A ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x252B ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x252C ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x252D ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x252E ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x252F ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2530 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2531 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2532 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2533 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2534 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2535 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2536 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2537 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2538 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2539 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x253A ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x253B ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x253C ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x253D ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x253E ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x253F ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2540 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2541 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2542 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2543 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2544 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2545 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2546 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2547 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2548 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2549 ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x254A ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x254B ; So    [76] BOX DRAWINGS LIGHT HORIZONTAL..BOX DRAWINGS HEAVY VERTICAL AND HORIZONTAL
                             #x2550 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2551 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2552 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2553 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2554 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2555 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2556 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2557 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2558 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2559 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x255A ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x255B ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x255C ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x255D ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x255E ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x255F ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2560 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2561 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2562 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2563 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2564 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2565 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2566 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2567 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2568 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2569 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x256A ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x256B ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x256C ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x256D ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x256E ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x256F ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2570 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2571 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2572 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2573 ; So    [36] BOX DRAWINGS DOUBLE HORIZONTAL..BOX DRAWINGS LIGHT DIAGONAL CROSS
                             #x2580 ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x2581 ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x2582 ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x2583 ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x2584 ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x2585 ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x2586 ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x2587 ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x2588 ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x2589 ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x258A ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x258B ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x258C ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x258D ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x258E ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x258F ; So    [16] UPPER HALF BLOCK..LEFT ONE EIGHTH BLOCK
                             #x2592 ; So     [4] MEDIUM SHADE..RIGHT ONE EIGHTH BLOCK
                             #x2593 ; So     [4] MEDIUM SHADE..RIGHT ONE EIGHTH BLOCK
                             #x2594 ; So     [4] MEDIUM SHADE..RIGHT ONE EIGHTH BLOCK
                             #x2595 ; So     [4] MEDIUM SHADE..RIGHT ONE EIGHTH BLOCK
                             #x25A0 ; So     [2] BLACK SQUARE..WHITE SQUARE
                             #x25A1 ; So     [2] BLACK SQUARE..WHITE SQUARE
                             #x25A3 ; So     [7] WHITE SQUARE CONTAINING BLACK SMALL SQUARE..SQUARE WITH DIAGONAL CROSSHATCH FILL
                             #x25A4 ; So     [7] WHITE SQUARE CONTAINING BLACK SMALL SQUARE..SQUARE WITH DIAGONAL CROSSHATCH FILL
                             #x25A5 ; So     [7] WHITE SQUARE CONTAINING BLACK SMALL SQUARE..SQUARE WITH DIAGONAL CROSSHATCH FILL
                             #x25A6 ; So     [7] WHITE SQUARE CONTAINING BLACK SMALL SQUARE..SQUARE WITH DIAGONAL CROSSHATCH FILL
                             #x25A7 ; So     [7] WHITE SQUARE CONTAINING BLACK SMALL SQUARE..SQUARE WITH DIAGONAL CROSSHATCH FILL
                             #x25A8 ; So     [7] WHITE SQUARE CONTAINING BLACK SMALL SQUARE..SQUARE WITH DIAGONAL CROSSHATCH FILL
                             #x25A9 ; So     [7] WHITE SQUARE CONTAINING BLACK SMALL SQUARE..SQUARE WITH DIAGONAL CROSSHATCH FILL
                             #x25B2 ; So     [2] BLACK UP-POINTING TRIANGLE..WHITE UP-POINTING TRIANGLE
                             #x25B3 ; So     [2] BLACK UP-POINTING TRIANGLE..WHITE UP-POINTING TRIANGLE
                             #x25B6 ; So         BLACK RIGHT-POINTING TRIANGLE
                             #x25B7 ; Sm         WHITE RIGHT-POINTING TRIANGLE
                             #x25BC ; So     [2] BLACK DOWN-POINTING TRIANGLE..WHITE DOWN-POINTING TRIANGLE
                             #x25BD ; So     [2] BLACK DOWN-POINTING TRIANGLE..WHITE DOWN-POINTING TRIANGLE
                             #x25C0 ; So         BLACK LEFT-POINTING TRIANGLE
                             #x25C1 ; Sm         WHITE LEFT-POINTING TRIANGLE
                             #x25C6 ; So     [3] BLACK DIAMOND..WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND
                             #x25C7 ; So     [3] BLACK DIAMOND..WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND
                             #x25C8 ; So     [3] BLACK DIAMOND..WHITE DIAMOND CONTAINING BLACK SMALL DIAMOND
                             #x25CB ; So         WHITE CIRCLE
                             #x25CE ; So     [4] BULLSEYE..CIRCLE WITH RIGHT HALF BLACK
                             #x25CF ; So     [4] BULLSEYE..CIRCLE WITH RIGHT HALF BLACK
                             #x25D0 ; So     [4] BULLSEYE..CIRCLE WITH RIGHT HALF BLACK
                             #x25D1 ; So     [4] BULLSEYE..CIRCLE WITH RIGHT HALF BLACK
                             #x25E2 ; So     [4] BLACK LOWER RIGHT TRIANGLE..BLACK UPPER RIGHT TRIANGLE
                             #x25E3 ; So     [4] BLACK LOWER RIGHT TRIANGLE..BLACK UPPER RIGHT TRIANGLE
                             #x25E4 ; So     [4] BLACK LOWER RIGHT TRIANGLE..BLACK UPPER RIGHT TRIANGLE
                             #x25E5 ; So     [4] BLACK LOWER RIGHT TRIANGLE..BLACK UPPER RIGHT TRIANGLE
                             #x25EF ; So         LARGE CIRCLE
                             #x2600 ; So     [5] BLACK SUN WITH RAYS..COMET
                             #x2601 ; So     [5] BLACK SUN WITH RAYS..COMET
                             #x2602 ; So     [5] BLACK SUN WITH RAYS..COMET
                             #x2603 ; So     [5] BLACK SUN WITH RAYS..COMET
                             #x2604 ; So     [5] BLACK SUN WITH RAYS..COMET
                             #x2605 ; So     [2] BLACK STAR..WHITE STAR
                             #x2606 ; So     [2] BLACK STAR..WHITE STAR
                             #x2607 ; So     [2] LIGHTNING..THUNDERSTORM
                             #x2608 ; So     [2] LIGHTNING..THUNDERSTORM
                             #x2609 ; So         SUN
                             #x260A ; So     [4] ASCENDING NODE..OPPOSITION
                             #x260B ; So     [4] ASCENDING NODE..OPPOSITION
                             #x260C ; So     [4] ASCENDING NODE..OPPOSITION
                             #x260D ; So     [4] ASCENDING NODE..OPPOSITION
                             #x260E ; So     [2] BLACK TELEPHONE..WHITE TELEPHONE
                             #x260F ; So     [2] BLACK TELEPHONE..WHITE TELEPHONE
                             #x2610 ; So     [4] BALLOT BOX..SALTIRE
                             #x2611 ; So     [4] BALLOT BOX..SALTIRE
                             #x2612 ; So     [4] BALLOT BOX..SALTIRE
                             #x2613 ; So     [4] BALLOT BOX..SALTIRE
                             #x2614 ; So     [2] UMBRELLA WITH RAIN DROPS..HOT BEVERAGE
                             #x2615 ; So     [2] UMBRELLA WITH RAIN DROPS..HOT BEVERAGE
                             #x2616 ; So     [6] WHITE SHOGI PIECE..BLACK RIGHT POINTING INDEX
                             #x2617 ; So     [6] WHITE SHOGI PIECE..BLACK RIGHT POINTING INDEX
                             #x2618 ; So     [6] WHITE SHOGI PIECE..BLACK RIGHT POINTING INDEX
                             #x2619 ; So     [6] WHITE SHOGI PIECE..BLACK RIGHT POINTING INDEX
                             #x261A ; So     [6] WHITE SHOGI PIECE..BLACK RIGHT POINTING INDEX
                             #x261B ; So     [6] WHITE SHOGI PIECE..BLACK RIGHT POINTING INDEX
                             #x261C ; So         WHITE LEFT POINTING INDEX
                             #x261D ; So         WHITE UP POINTING INDEX
                             #x261E ; So         WHITE RIGHT POINTING INDEX
                             #x261F ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2620 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2621 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2622 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2623 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2624 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2625 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2626 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2627 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2628 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2629 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x262A ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x262B ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x262C ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x262D ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x262E ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x262F ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2630 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2631 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2632 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2633 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2634 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2635 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2636 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2637 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2638 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2639 ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x263A ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x263B ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x263C ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x263D ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x263E ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x263F ; So    [33] WHITE DOWN POINTING INDEX..MERCURY
                             #x2640 ; So         FEMALE SIGN
                             #x2641 ; So         EARTH
                             #x2642 ; So         MALE SIGN
                             #x2643 ; So     [5] JUPITER..PLUTO
                             #x2644 ; So     [5] JUPITER..PLUTO
                             #x2645 ; So     [5] JUPITER..PLUTO
                             #x2646 ; So     [5] JUPITER..PLUTO
                             #x2647 ; So     [5] JUPITER..PLUTO
                             #x2648 ; So    [12] ARIES..PISCES
                             #x2649 ; So    [12] ARIES..PISCES
                             #x264A ; So    [12] ARIES..PISCES
                             #x264B ; So    [12] ARIES..PISCES
                             #x264C ; So    [12] ARIES..PISCES
                             #x264D ; So    [12] ARIES..PISCES
                             #x264E ; So    [12] ARIES..PISCES
                             #x264F ; So    [12] ARIES..PISCES
                             #x2650 ; So    [12] ARIES..PISCES
                             #x2651 ; So    [12] ARIES..PISCES
                             #x2652 ; So    [12] ARIES..PISCES
                             #x2653 ; So    [12] ARIES..PISCES
                             #x2654 ; So    [12] WHITE CHESS KING..BLACK CHESS PAWN
                             #x2655 ; So    [12] WHITE CHESS KING..BLACK CHESS PAWN
                             #x2656 ; So    [12] WHITE CHESS KING..BLACK CHESS PAWN
                             #x2657 ; So    [12] WHITE CHESS KING..BLACK CHESS PAWN
                             #x2658 ; So    [12] WHITE CHESS KING..BLACK CHESS PAWN
                             #x2659 ; So    [12] WHITE CHESS KING..BLACK CHESS PAWN
                             #x265A ; So    [12] WHITE CHESS KING..BLACK CHESS PAWN
                             #x265B ; So    [12] WHITE CHESS KING..BLACK CHESS PAWN
                             #x265C ; So    [12] WHITE CHESS KING..BLACK CHESS PAWN
                             #x265D ; So    [12] WHITE CHESS KING..BLACK CHESS PAWN
                             #x265E ; So    [12] WHITE CHESS KING..BLACK CHESS PAWN
                             #x265F ; So    [12] WHITE CHESS KING..BLACK CHESS PAWN
                             #x2660 ; So     [2] BLACK SPADE SUIT..WHITE HEART SUIT
                             #x2661 ; So     [2] BLACK SPADE SUIT..WHITE HEART SUIT
                             #x2662 ; So         WHITE DIAMOND SUIT
                             #x2663 ; So     [3] BLACK CLUB SUIT..BLACK HEART SUIT
                             #x2664 ; So     [3] BLACK CLUB SUIT..BLACK HEART SUIT
                             #x2665 ; So     [3] BLACK CLUB SUIT..BLACK HEART SUIT
                             #x2666 ; So         BLACK DIAMOND SUIT
                             #x2667 ; So     [4] WHITE CLUB SUIT..EIGHTH NOTE
                             #x2668 ; So     [4] WHITE CLUB SUIT..EIGHTH NOTE
                             #x2669 ; So     [4] WHITE CLUB SUIT..EIGHTH NOTE
                             #x266A ; So     [4] WHITE CLUB SUIT..EIGHTH NOTE
                             #x266B ; So         BEAMED EIGHTH NOTES
                             #x266C ; So     [2] BEAMED SIXTEENTH NOTES..MUSIC FLAT SIGN
                             #x266D ; So     [2] BEAMED SIXTEENTH NOTES..MUSIC FLAT SIGN
                             #x266E ; So         MUSIC NATURAL SIGN
                             #x266F ; Sm         MUSIC SHARP SIGN
                             #x2670 ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x2671 ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x2672 ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x2673 ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x2674 ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x2675 ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x2676 ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x2677 ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x2678 ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x2679 ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x267A ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x267B ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x267C ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x267D ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x267E ; So    [15] WEST SYRIAC CROSS..PERMANENT PAPER SIGN
                             #x267F ; So         WHEELCHAIR SYMBOL
                             #x2680 ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x2681 ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x2682 ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x2683 ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x2684 ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x2685 ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x2686 ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x2687 ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x2688 ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x2689 ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x268A ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x268B ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x268C ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x268D ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x268E ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x268F ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x2690 ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x2691 ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x2692 ; So    [19] DIE FACE-1..HAMMER AND PICK
                             #x2693 ; So         ANCHOR
                             #x2694 ; So    [10] CROSSED SWORDS..OUTLINED WHITE STAR
                             #x2695 ; So    [10] CROSSED SWORDS..OUTLINED WHITE STAR
                             #x2696 ; So    [10] CROSSED SWORDS..OUTLINED WHITE STAR
                             #x2697 ; So    [10] CROSSED SWORDS..OUTLINED WHITE STAR
                             #x2698 ; So    [10] CROSSED SWORDS..OUTLINED WHITE STAR
                             #x2699 ; So    [10] CROSSED SWORDS..OUTLINED WHITE STAR
                             #x269A ; So    [10] CROSSED SWORDS..OUTLINED WHITE STAR
                             #x269B ; So    [10] CROSSED SWORDS..OUTLINED WHITE STAR
                             #x269C ; So    [10] CROSSED SWORDS..OUTLINED WHITE STAR
                             #x269D ; So    [10] CROSSED SWORDS..OUTLINED WHITE STAR
                             #x269E ; So     [2] THREE LINES CONVERGING RIGHT..THREE LINES CONVERGING LEFT
                             #x269F ; So     [2] THREE LINES CONVERGING RIGHT..THREE LINES CONVERGING LEFT
                             #x26A0 ; So         WARNING SIGN
                             #x26A1 ; So         HIGH VOLTAGE SIGN
                             #x26A2 ; So     [8] DOUBLED FEMALE SIGN..HORIZONTAL MALE WITH STROKE SIGN
                             #x26A3 ; So     [8] DOUBLED FEMALE SIGN..HORIZONTAL MALE WITH STROKE SIGN
                             #x26A4 ; So     [8] DOUBLED FEMALE SIGN..HORIZONTAL MALE WITH STROKE SIGN
                             #x26A5 ; So     [8] DOUBLED FEMALE SIGN..HORIZONTAL MALE WITH STROKE SIGN
                             #x26A6 ; So     [8] DOUBLED FEMALE SIGN..HORIZONTAL MALE WITH STROKE SIGN
                             #x26A7 ; So     [8] DOUBLED FEMALE SIGN..HORIZONTAL MALE WITH STROKE SIGN
                             #x26A8 ; So     [8] DOUBLED FEMALE SIGN..HORIZONTAL MALE WITH STROKE SIGN
                             #x26A9 ; So     [8] DOUBLED FEMALE SIGN..HORIZONTAL MALE WITH STROKE SIGN
                             #x26AA ; So     [2] MEDIUM WHITE CIRCLE..MEDIUM BLACK CIRCLE
                             #x26AB ; So     [2] MEDIUM WHITE CIRCLE..MEDIUM BLACK CIRCLE
                             #x26AC ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26AD ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26AE ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26AF ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26B0 ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26B1 ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26B2 ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26B3 ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26B4 ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26B5 ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26B6 ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26B7 ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26B8 ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26B9 ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26BA ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26BB ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26BC ; So    [17] MEDIUM SMALL WHITE CIRCLE..SESQUIQUADRATE
                             #x26BD ; So     [2] SOCCER BALL..BASEBALL
                             #x26BE ; So     [2] SOCCER BALL..BASEBALL
                             #x26BF ; So         SQUARED KEY
                             #x26C0 ; So     [4] WHITE DRAUGHTS MAN..BLACK DRAUGHTS KING
                             #x26C1 ; So     [4] WHITE DRAUGHTS MAN..BLACK DRAUGHTS KING
                             #x26C2 ; So     [4] WHITE DRAUGHTS MAN..BLACK DRAUGHTS KING
                             #x26C3 ; So     [4] WHITE DRAUGHTS MAN..BLACK DRAUGHTS KING
                             #x26C4 ; So     [2] SNOWMAN WITHOUT SNOW..SUN BEHIND CLOUD
                             #x26C5 ; So     [2] SNOWMAN WITHOUT SNOW..SUN BEHIND CLOUD
                             #x26C6 ; So     [8] RAIN..DISABLED CAR
                             #x26C7 ; So     [8] RAIN..DISABLED CAR
                             #x26C8 ; So     [8] RAIN..DISABLED CAR
                             #x26C9 ; So     [8] RAIN..DISABLED CAR
                             #x26CA ; So     [8] RAIN..DISABLED CAR
                             #x26CB ; So     [8] RAIN..DISABLED CAR
                             #x26CC ; So     [8] RAIN..DISABLED CAR
                             #x26CD ; So     [8] RAIN..DISABLED CAR
                             #x26CE ; So         OPHIUCHUS
                             #x26CF ; So     [5] PICK..CHAINS
                             #x26D0 ; So     [5] PICK..CHAINS
                             #x26D1 ; So     [5] PICK..CHAINS
                             #x26D2 ; So     [5] PICK..CHAINS
                             #x26D3 ; So     [5] PICK..CHAINS
                             #x26D4 ; So         NO ENTRY
                             #x26D5 ; So    [13] ALTERNATE ONE-WAY LEFT WAY TRAFFIC..RESTRICTED LEFT ENTRY-2
                             #x26D6 ; So    [13] ALTERNATE ONE-WAY LEFT WAY TRAFFIC..RESTRICTED LEFT ENTRY-2
                             #x26D7 ; So    [13] ALTERNATE ONE-WAY LEFT WAY TRAFFIC..RESTRICTED LEFT ENTRY-2
                             #x26D8 ; So    [13] ALTERNATE ONE-WAY LEFT WAY TRAFFIC..RESTRICTED LEFT ENTRY-2
                             #x26D9 ; So    [13] ALTERNATE ONE-WAY LEFT WAY TRAFFIC..RESTRICTED LEFT ENTRY-2
                             #x26DA ; So    [13] ALTERNATE ONE-WAY LEFT WAY TRAFFIC..RESTRICTED LEFT ENTRY-2
                             #x26DB ; So    [13] ALTERNATE ONE-WAY LEFT WAY TRAFFIC..RESTRICTED LEFT ENTRY-2
                             #x26DC ; So    [13] ALTERNATE ONE-WAY LEFT WAY TRAFFIC..RESTRICTED LEFT ENTRY-2
                             #x26DD ; So    [13] ALTERNATE ONE-WAY LEFT WAY TRAFFIC..RESTRICTED LEFT ENTRY-2
                             #x26DE ; So    [13] ALTERNATE ONE-WAY LEFT WAY TRAFFIC..RESTRICTED LEFT ENTRY-2
                             #x26DF ; So    [13] ALTERNATE ONE-WAY LEFT WAY TRAFFIC..RESTRICTED LEFT ENTRY-2
                             #x26E0 ; So    [13] ALTERNATE ONE-WAY LEFT WAY TRAFFIC..RESTRICTED LEFT ENTRY-2
                             #x26E1 ; So    [13] ALTERNATE ONE-WAY LEFT WAY TRAFFIC..RESTRICTED LEFT ENTRY-2
                             #x26E2 ; So         ASTRONOMICAL SYMBOL FOR URANUS
                             #x26E3 ; So         HEAVY CIRCLE WITH STROKE AND TWO DOTS ABOVE
                             #x26E4 ; So     [4] PENTAGRAM..INVERTED PENTAGRAM
                             #x26E5 ; So     [4] PENTAGRAM..INVERTED PENTAGRAM
                             #x26E6 ; So     [4] PENTAGRAM..INVERTED PENTAGRAM
                             #x26E7 ; So     [4] PENTAGRAM..INVERTED PENTAGRAM
                             #x26E8 ; So     [2] BLACK CROSS ON SHIELD..SHINTO SHRINE
                             #x26E9 ; So     [2] BLACK CROSS ON SHIELD..SHINTO SHRINE
                             #x26EA ; So         CHURCH
                             #x26EB ; So     [7] CASTLE..UMBRELLA ON GROUND
                             #x26EC ; So     [7] CASTLE..UMBRELLA ON GROUND
                             #x26ED ; So     [7] CASTLE..UMBRELLA ON GROUND
                             #x26EE ; So     [7] CASTLE..UMBRELLA ON GROUND
                             #x26EF ; So     [7] CASTLE..UMBRELLA ON GROUND
                             #x26F0 ; So     [7] CASTLE..UMBRELLA ON GROUND
                             #x26F1 ; So     [7] CASTLE..UMBRELLA ON GROUND
                             #x26F2 ; So     [2] FOUNTAIN..FLAG IN HOLE
                             #x26F3 ; So     [2] FOUNTAIN..FLAG IN HOLE
                             #x26F4 ; So         FERRY
                             #x26F5 ; So         SAILBOAT
                             #x26F6 ; So     [4] SQUARE FOUR CORNERS..PERSON WITH BALL
                             #x26F7 ; So     [4] SQUARE FOUR CORNERS..PERSON WITH BALL
                             #x26F8 ; So     [4] SQUARE FOUR CORNERS..PERSON WITH BALL
                             #x26F9 ; So     [4] SQUARE FOUR CORNERS..PERSON WITH BALL
                             #x26FA ; So         TENT
                             #x26FB ; So     [2] JAPANESE BANK SYMBOL..HEADSTONE GRAVEYARD SYMBOL
                             #x26FC ; So     [2] JAPANESE BANK SYMBOL..HEADSTONE GRAVEYARD SYMBOL
                             #x26FD ; So         FUEL PUMP
                             #x26FE ; So     [2] CUP ON BLACK SQUARE..WHITE FLAG WITH HORIZONTAL MIDDLE BLACK STRIPE
                             #x26FF ; So     [2] CUP ON BLACK SQUARE..WHITE FLAG WITH HORIZONTAL MIDDLE BLACK STRIPE
                             #x2700 ; So     [5] BLACK SAFETY SCISSORS..WHITE SCISSORS
                             #x2701 ; So     [5] BLACK SAFETY SCISSORS..WHITE SCISSORS
                             #x2702 ; So     [5] BLACK SAFETY SCISSORS..WHITE SCISSORS
                             #x2703 ; So     [5] BLACK SAFETY SCISSORS..WHITE SCISSORS
                             #x2704 ; So     [5] BLACK SAFETY SCISSORS..WHITE SCISSORS
                             #x2705 ; So         WHITE HEAVY CHECK MARK
                             #x2706 ; So     [4] TELEPHONE LOCATION SIGN..ENVELOPE
                             #x2707 ; So     [4] TELEPHONE LOCATION SIGN..ENVELOPE
                             #x2708 ; So     [4] TELEPHONE LOCATION SIGN..ENVELOPE
                             #x2709 ; So     [4] TELEPHONE LOCATION SIGN..ENVELOPE
                             #x270A ; So     [2] RAISED FIST..RAISED HAND
                             #x270B ; So     [2] RAISED FIST..RAISED HAND
                             #x270C ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x270D ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x270E ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x270F ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2710 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2711 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2712 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2713 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2714 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2715 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2716 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2717 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2718 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2719 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x271A ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x271B ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x271C ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x271D ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x271E ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x271F ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2720 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2721 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2722 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2723 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2724 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2725 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2726 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2727 ; So    [28] VICTORY HAND..WHITE FOUR POINTED STAR
                             #x2728 ; So         SPARKLES
                             #x2729 ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x272A ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x272B ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x272C ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x272D ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x272E ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x272F ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x2730 ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x2731 ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x2732 ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x2733 ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x2734 ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x2735 ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x2736 ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x2737 ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x2738 ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x2739 ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x273A ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x273B ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x273C ; So    [20] STRESS OUTLINED WHITE STAR..OPEN CENTRE TEARDROP-SPOKED ASTERISK
                             #x273D ; So         HEAVY TEARDROP-SPOKED ASTERISK
                             #x273E ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x273F ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x2740 ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x2741 ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x2742 ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x2743 ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x2744 ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x2745 ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x2746 ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x2747 ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x2748 ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x2749 ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x274A ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x274B ; So    [14] SIX PETALLED BLACK AND WHITE FLORETTE..HEAVY EIGHT TEARDROP-SPOKED PROPELLER ASTERISK
                             #x274C ; So         CROSS MARK
                             #x274D ; So         SHADOWED WHITE CIRCLE
                             #x274E ; So         NEGATIVE SQUARED CROSS MARK
                             #x274F ; So     [4] LOWER RIGHT DROP-SHADOWED WHITE SQUARE..UPPER RIGHT SHADOWED WHITE SQUARE
                             #x2750 ; So     [4] LOWER RIGHT DROP-SHADOWED WHITE SQUARE..UPPER RIGHT SHADOWED WHITE SQUARE
                             #x2751 ; So     [4] LOWER RIGHT DROP-SHADOWED WHITE SQUARE..UPPER RIGHT SHADOWED WHITE SQUARE
                             #x2752 ; So     [4] LOWER RIGHT DROP-SHADOWED WHITE SQUARE..UPPER RIGHT SHADOWED WHITE SQUARE
                             #x2753 ; So     [3] BLACK QUESTION MARK ORNAMENT..WHITE EXCLAMATION MARK ORNAMENT
                             #x2754 ; So     [3] BLACK QUESTION MARK ORNAMENT..WHITE EXCLAMATION MARK ORNAMENT
                             #x2755 ; So     [3] BLACK QUESTION MARK ORNAMENT..WHITE EXCLAMATION MARK ORNAMENT
                             #x2756 ; So         BLACK DIAMOND MINUS WHITE X
                             #x2757 ; So         HEAVY EXCLAMATION MARK SYMBOL
                             #x2758 ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x2759 ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x275A ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x275B ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x275C ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x275D ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x275E ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x275F ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x2760 ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x2761 ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x2762 ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x2763 ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x2764 ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x2765 ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x2766 ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x2767 ; So    [16] LIGHT VERTICAL BAR..ROTATED FLORAL HEART BULLET
                             #x2768 ; Ps         MEDIUM LEFT PARENTHESIS ORNAMENT
                             #x2769 ; Pe         MEDIUM RIGHT PARENTHESIS ORNAMENT
                             #x276A ; Ps         MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT
                             #x276B ; Pe         MEDIUM FLATTENED RIGHT PARENTHESIS ORNAMENT
                             #x276C ; Ps         MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT
                             #x276D ; Pe         MEDIUM RIGHT-POINTING ANGLE BRACKET ORNAMENT
                             #x276E ; Ps         HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT
                             #x276F ; Pe         HEAVY RIGHT-POINTING ANGLE QUOTATION MARK ORNAMENT
                             #x2770 ; Ps         HEAVY LEFT-POINTING ANGLE BRACKET ORNAMENT
                             #x2771 ; Pe         HEAVY RIGHT-POINTING ANGLE BRACKET ORNAMENT
                             #x2772 ; Ps         LIGHT LEFT TORTOISE SHELL BRACKET ORNAMENT
                             #x2773 ; Pe         LIGHT RIGHT TORTOISE SHELL BRACKET ORNAMENT
                             #x2774 ; Ps         MEDIUM LEFT CURLY BRACKET ORNAMENT
                             #x2775 ; Pe         MEDIUM RIGHT CURLY BRACKET ORNAMENT
                             #x2776 ; No    [10] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED NUMBER TEN
                             #x2777 ; No    [10] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED NUMBER TEN
                             #x2778 ; No    [10] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED NUMBER TEN
                             #x2779 ; No    [10] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED NUMBER TEN
                             #x277A ; No    [10] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED NUMBER TEN
                             #x277B ; No    [10] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED NUMBER TEN
                             #x277C ; No    [10] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED NUMBER TEN
                             #x277D ; No    [10] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED NUMBER TEN
                             #x277E ; No    [10] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED NUMBER TEN
                             #x277F ; No    [10] DINGBAT NEGATIVE CIRCLED DIGIT ONE..DINGBAT NEGATIVE CIRCLED NUMBER TEN
                             #x2780 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2781 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2782 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2783 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2784 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2785 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2786 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2787 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2788 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2789 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x278A ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x278B ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x278C ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x278D ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x278E ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x278F ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2790 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2791 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2792 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2793 ; No    [20] DINGBAT CIRCLED SANS-SERIF DIGIT ONE..DINGBAT NEGATIVE CIRCLED SANS-SERIF NUMBER TEN
                             #x2794 ; So         HEAVY WIDE-HEADED RIGHTWARDS ARROW
                             #x2795 ; So     [3] HEAVY PLUS SIGN..HEAVY DIVISION SIGN
                             #x2796 ; So     [3] HEAVY PLUS SIGN..HEAVY DIVISION SIGN
                             #x2797 ; So     [3] HEAVY PLUS SIGN..HEAVY DIVISION SIGN
                             #x2798 ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x2799 ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x279A ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x279B ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x279C ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x279D ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x279E ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x279F ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27A0 ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27A1 ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27A2 ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27A3 ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27A4 ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27A5 ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27A6 ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27A7 ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27A8 ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27A9 ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27AA ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27AB ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27AC ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27AD ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27AE ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27AF ; So    [24] HEAVY SOUTH EAST ARROW..NOTCHED LOWER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW
                             #x27B0 ; So         CURLY LOOP
                             #x27B1 ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27B2 ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27B3 ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27B4 ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27B5 ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27B6 ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27B7 ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27B8 ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27B9 ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27BA ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27BB ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27BC ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27BD ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27BE ; So    [14] NOTCHED UPPER RIGHT-SHADOWED WHITE RIGHTWARDS ARROW..OPEN-OUTLINED RIGHTWARDS ARROW
                             #x27BF ; So         DOUBLE CURLY LOOP
                             #x27C0 ; Sm     [5] THREE DIMENSIONAL ANGLE..OPEN SUPERSET
                             #x27C1 ; Sm     [5] THREE DIMENSIONAL ANGLE..OPEN SUPERSET
                             #x27C2 ; Sm     [5] THREE DIMENSIONAL ANGLE..OPEN SUPERSET
                             #x27C3 ; Sm     [5] THREE DIMENSIONAL ANGLE..OPEN SUPERSET
                             #x27C4 ; Sm     [5] THREE DIMENSIONAL ANGLE..OPEN SUPERSET
                             #x27C5 ; Ps         LEFT S-SHAPED BAG DELIMITER
                             #x27C6 ; Pe         RIGHT S-SHAPED BAG DELIMITER
                             #x27C7 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27C8 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27C9 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27CA ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27CB ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27CC ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27CD ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27CE ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27CF ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27D0 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27D1 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27D2 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27D3 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27D4 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27D5 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27D6 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27D7 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27D8 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27D9 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27DA ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27DB ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27DC ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27DD ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27DE ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27DF ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27E0 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27E1 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27E2 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27E3 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27E4 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27E5 ; Sm    [31] OR WITH DOT INSIDE..WHITE SQUARE WITH RIGHTWARDS TICK
                             #x27EE ; Ps         MATHEMATICAL LEFT FLATTENED PARENTHESIS
                             #x27EF ; Pe         MATHEMATICAL RIGHT FLATTENED PARENTHESIS
                             #x27F0 ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27F1 ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27F2 ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27F3 ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27F4 ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27F5 ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27F6 ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27F7 ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27F8 ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27F9 ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27FA ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27FB ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27FC ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27FD ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27FE ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x27FF ; Sm    [16] UPWARDS QUADRUPLE ARROW..LONG RIGHTWARDS SQUIGGLE ARROW
                             #x2B56 ; So     [4] HEAVY OVAL WITH OVAL INSIDE..HEAVY CIRCLED SALTIRE
                             #x2B57 ; So     [4] HEAVY OVAL WITH OVAL INSIDE..HEAVY CIRCLED SALTIRE
                             #x2B58 ; So     [4] HEAVY OVAL WITH OVAL INSIDE..HEAVY CIRCLED SALTIRE
                             #x2B59 ; So     [4] HEAVY OVAL WITH OVAL INSIDE..HEAVY CIRCLED SALTIRE
                             #x3248 ; No     [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
                             #x3249 ; No     [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
                             #x324A ; No     [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
                             #x324B ; No     [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
                             #x324C ; No     [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
                             #x324D ; No     [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
                             #x324E ; No     [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
                             #x324F ; No     [8] CIRCLED NUMBER TEN ON BLACK SQUARE..CIRCLED NUMBER EIGHTY ON BLACK SQUARE
                             #xFFFD ; So         REPLACEMENT CHARACTER
                             #x1F000 ; So     [4] MAHJONG TILE EAST WIND..MAHJONG TILE NORTH WIND
                             #x1F001 ; So     [4] MAHJONG TILE EAST WIND..MAHJONG TILE NORTH WIND
                             #x1F002 ; So     [4] MAHJONG TILE EAST WIND..MAHJONG TILE NORTH WIND
                             #x1F003 ; So     [4] MAHJONG TILE EAST WIND..MAHJONG TILE NORTH WIND
                             #x1F004 ; So         MAHJONG TILE RED DRAGON
                             #x1F005 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F006 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F007 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F008 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F009 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F00A ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F00B ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F00C ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F00D ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F00E ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F00F ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F010 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F011 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F012 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F013 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F014 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F015 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F016 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F017 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F018 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F019 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F01A ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F01B ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F01C ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F01D ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F01E ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F01F ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F020 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F021 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F022 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F023 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F024 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F025 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F026 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F027 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F028 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F029 ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F02A ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F02B ; So    [39] MAHJONG TILE GREEN DRAGON..MAHJONG TILE BACK
                             #x1F030 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F031 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F032 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F033 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F034 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F035 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F036 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F037 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F038 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F039 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F03A ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F03B ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F03C ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F03D ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F03E ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F03F ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F040 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F041 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F042 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F043 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F044 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F045 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F046 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F047 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F048 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F049 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F04A ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F04B ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F04C ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F04D ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F04E ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F04F ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F050 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F051 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F052 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F053 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F054 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F055 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F056 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F057 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F058 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F059 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F05A ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F05B ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F05C ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F05D ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F05E ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F05F ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F060 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F061 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F062 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F063 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F064 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F065 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F066 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F067 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F068 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F069 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F06A ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F06B ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F06C ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F06D ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F06E ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F06F ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F070 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F071 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F072 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F073 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F074 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F075 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F076 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F077 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F078 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F079 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F07A ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F07B ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F07C ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F07D ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F07E ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F07F ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F080 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F081 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F082 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F083 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F084 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F085 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F086 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F087 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F088 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F089 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F08A ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F08B ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F08C ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F08D ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F08E ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F08F ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F090 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F091 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F092 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F093 ; So   [100] DOMINO TILE HORIZONTAL BACK..DOMINO TILE VERTICAL-06-06
                             #x1F0A0 ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0A1 ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0A2 ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0A3 ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0A4 ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0A5 ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0A6 ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0A7 ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0A8 ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0A9 ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0AA ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0AB ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0AC ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0AD ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0AE ; So    [15] PLAYING CARD BACK..PLAYING CARD KING OF SPADES
                             #x1F0B1 ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0B2 ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0B3 ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0B4 ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0B5 ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0B6 ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0B7 ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0B8 ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0B9 ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0BA ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0BB ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0BC ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0BD ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0BE ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0BF ; So    [15] PLAYING CARD ACE OF HEARTS..PLAYING CARD RED JOKER
                             #x1F0C1 ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0C2 ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0C3 ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0C4 ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0C5 ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0C6 ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0C7 ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0C8 ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0C9 ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0CA ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0CB ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0CC ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0CD ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0CE ; So    [14] PLAYING CARD ACE OF DIAMONDS..PLAYING CARD KING OF DIAMONDS
                             #x1F0CF ; So         PLAYING CARD BLACK JOKER
                             #x1F0D1 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0D2 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0D3 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0D4 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0D5 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0D6 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0D7 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0D8 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0D9 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0DA ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0DB ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0DC ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0DD ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0DE ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0DF ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0E0 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0E1 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0E2 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0E3 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0E4 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0E5 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0E6 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0E7 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0E8 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0E9 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0EA ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0EB ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0EC ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0ED ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0EE ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0EF ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0F0 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0F1 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0F2 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0F3 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0F4 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F0F5 ; So    [37] PLAYING CARD ACE OF CLUBS..PLAYING CARD TRUMP-21
                             #x1F100 ; No    [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
                             #x1F101 ; No    [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
                             #x1F102 ; No    [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
                             #x1F103 ; No    [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
                             #x1F104 ; No    [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
                             #x1F105 ; No    [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
                             #x1F106 ; No    [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
                             #x1F107 ; No    [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
                             #x1F108 ; No    [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
                             #x1F109 ; No    [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
                             #x1F10A ; No    [11] DIGIT ZERO FULL STOP..DIGIT NINE COMMA
                             #x1F10B ; No     [2] DINGBAT CIRCLED SANS-SERIF DIGIT ZERO..DINGBAT NEGATIVE CIRCLED SANS-SERIF DIGIT ZERO
                             #x1F10C ; No     [2] DINGBAT CIRCLED SANS-SERIF DIGIT ZERO..DINGBAT NEGATIVE CIRCLED SANS-SERIF DIGIT ZERO
                             #x1F10D ; So     [3] CIRCLED ZERO WITH SLASH..CIRCLED DOLLAR SIGN WITH OVERLAID BACKSLASH
                             #x1F10E ; So     [3] CIRCLED ZERO WITH SLASH..CIRCLED DOLLAR SIGN WITH OVERLAID BACKSLASH
                             #x1F10F ; So     [3] CIRCLED ZERO WITH SLASH..CIRCLED DOLLAR SIGN WITH OVERLAID BACKSLASH
                             #x1F110 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F111 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F112 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F113 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F114 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F115 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F116 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F117 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F118 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F119 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F11A ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F11B ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F11C ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F11D ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F11E ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F11F ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F120 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F121 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F122 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F123 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F124 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F125 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F126 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F127 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F128 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F129 ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F12A ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F12B ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F12C ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F12D ; So    [30] PARENTHESIZED LATIN CAPITAL LETTER A..CIRCLED CD
                             #x1F12E ; So     [2] CIRCLED WZ..COPYLEFT SYMBOL
                             #x1F12F ; So     [2] CIRCLED WZ..COPYLEFT SYMBOL
                             #x1F130 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F131 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F132 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F133 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F134 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F135 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F136 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F137 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F138 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F139 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F13A ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F13B ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F13C ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F13D ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F13E ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F13F ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F140 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F141 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F142 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F143 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F144 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F145 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F146 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F147 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F148 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F149 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F14A ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F14B ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F14C ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F14D ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F14E ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F14F ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F150 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F151 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F152 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F153 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F154 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F155 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F156 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F157 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F158 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F159 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F15A ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F15B ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F15C ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F15D ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F15E ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F15F ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F160 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F161 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F162 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F163 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F164 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F165 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F166 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F167 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F168 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F169 ; So    [58] SQUARED LATIN CAPITAL LETTER A..NEGATIVE CIRCLED LATIN CAPITAL LETTER Z
                             #x1F16A ; So     [6] RAISED MC SIGN..CIRCLED HUMAN FIGURE
                             #x1F16B ; So     [6] RAISED MC SIGN..CIRCLED HUMAN FIGURE
                             #x1F16C ; So     [6] RAISED MC SIGN..CIRCLED HUMAN FIGURE
                             #x1F16D ; So     [6] RAISED MC SIGN..CIRCLED HUMAN FIGURE
                             #x1F16E ; So     [6] RAISED MC SIGN..CIRCLED HUMAN FIGURE
                             #x1F16F ; So     [6] RAISED MC SIGN..CIRCLED HUMAN FIGURE
                             #x1F170 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F171 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F172 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F173 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F174 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F175 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F176 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F177 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F178 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F179 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F17A ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F17B ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F17C ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F17D ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F17E ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F17F ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F180 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F181 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F182 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F183 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F184 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F185 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F186 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F187 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F188 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F189 ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F18A ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F18B ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F18C ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F18D ; So    [30] NEGATIVE SQUARED LATIN CAPITAL LETTER A..NEGATIVE SQUARED SA
                             #x1F18E ; So         NEGATIVE SQUARED AB
                             #x1F18F ; So     [2] NEGATIVE SQUARED WC..SQUARE DJ
                             #x1F190 ; So     [2] NEGATIVE SQUARED WC..SQUARE DJ
                             #x1F191 ; So    [10] SQUARED CL..SQUARED VS
                             #x1F192 ; So    [10] SQUARED CL..SQUARED VS
                             #x1F193 ; So    [10] SQUARED CL..SQUARED VS
                             #x1F194 ; So    [10] SQUARED CL..SQUARED VS
                             #x1F195 ; So    [10] SQUARED CL..SQUARED VS
                             #x1F196 ; So    [10] SQUARED CL..SQUARED VS
                             #x1F197 ; So    [10] SQUARED CL..SQUARED VS
                             #x1F198 ; So    [10] SQUARED CL..SQUARED VS
                             #x1F199 ; So    [10] SQUARED CL..SQUARED VS
                             #x1F19A ; So    [10] SQUARED CL..SQUARED VS
                             #x1F19B ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F19C ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F19D ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F19E ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F19F ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1A0 ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1A1 ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1A2 ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1A3 ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1A4 ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1A5 ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1A6 ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1A7 ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1A8 ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1A9 ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1AA ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1AB ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1AC ; So    [18] SQUARED THREE D..SQUARED VOD
                             #x1F1AD ; So         MASK WORK SYMBOL
                             #x1F1E6 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1E7 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1E8 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1E9 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1EA ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1EB ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1EC ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1ED ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1EE ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1EF ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1F0 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1F1 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1F2 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1F3 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1F4 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1F5 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1F6 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1F7 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1F8 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1F9 ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1FA ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1FB ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1FC ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1FD ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1FE ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F1FF ; So    [26] REGIONAL INDICATOR SYMBOL LETTER A..REGIONAL INDICATOR SYMBOL LETTER Z
                             #x1F200 ; So     [3] SQUARE HIRAGANA HOKA..SQUARED KATAKANA SA
                             #x1F201 ; So     [3] SQUARE HIRAGANA HOKA..SQUARED KATAKANA SA
                             #x1F202 ; So     [3] SQUARE HIRAGANA HOKA..SQUARED KATAKANA SA
                             #x1F210 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F211 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F212 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F213 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F214 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F215 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F216 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F217 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F218 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F219 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F21A ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F21B ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F21C ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F21D ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F21E ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F21F ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F220 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F221 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F222 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F223 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F224 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F225 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F226 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F227 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F228 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F229 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F22A ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F22B ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F22C ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F22D ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F22E ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F22F ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F230 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F231 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F232 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F233 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F234 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F235 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F236 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F237 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F238 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F239 ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F23A ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F23B ; So    [44] SQUARED CJK UNIFIED IDEOGRAPH-624B..SQUARED CJK UNIFIED IDEOGRAPH-914D
                             #x1F240 ; So     [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
                             #x1F241 ; So     [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
                             #x1F242 ; So     [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
                             #x1F243 ; So     [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
                             #x1F244 ; So     [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
                             #x1F245 ; So     [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
                             #x1F246 ; So     [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
                             #x1F247 ; So     [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
                             #x1F248 ; So     [9] TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-672C..TORTOISE SHELL BRACKETED CJK UNIFIED IDEOGRAPH-6557
                             #x1F250 ; So     [2] CIRCLED IDEOGRAPH ADVANTAGE..CIRCLED IDEOGRAPH ACCEPT
                             #x1F251 ; So     [2] CIRCLED IDEOGRAPH ADVANTAGE..CIRCLED IDEOGRAPH ACCEPT
                             #x1F260 ; So     [6] ROUNDED SYMBOL FOR FU..ROUNDED SYMBOL FOR CAI
                             #x1F261 ; So     [6] ROUNDED SYMBOL FOR FU..ROUNDED SYMBOL FOR CAI
                             #x1F262 ; So     [6] ROUNDED SYMBOL FOR FU..ROUNDED SYMBOL FOR CAI
                             #x1F263 ; So     [6] ROUNDED SYMBOL FOR FU..ROUNDED SYMBOL FOR CAI
                             #x1F264 ; So     [6] ROUNDED SYMBOL FOR FU..ROUNDED SYMBOL FOR CAI
                             #x1F265 ; So     [6] ROUNDED SYMBOL FOR FU..ROUNDED SYMBOL FOR CAI
                             #x1F300 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F301 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F302 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F303 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F304 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F305 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F306 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F307 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F308 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F309 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F30A ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F30B ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F30C ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F30D ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F30E ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F30F ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F310 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F311 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F312 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F313 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F314 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F315 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F316 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F317 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F318 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F319 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F31A ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F31B ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F31C ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F31D ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F31E ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F31F ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F320 ; So    [33] CYCLONE..SHOOTING STAR
                             #x1F321 ; So    [12] THERMOMETER..WIND BLOWING FACE
                             #x1F322 ; So    [12] THERMOMETER..WIND BLOWING FACE
                             #x1F323 ; So    [12] THERMOMETER..WIND BLOWING FACE
                             #x1F324 ; So    [12] THERMOMETER..WIND BLOWING FACE
                             #x1F325 ; So    [12] THERMOMETER..WIND BLOWING FACE
                             #x1F326 ; So    [12] THERMOMETER..WIND BLOWING FACE
                             #x1F327 ; So    [12] THERMOMETER..WIND BLOWING FACE
                             #x1F328 ; So    [12] THERMOMETER..WIND BLOWING FACE
                             #x1F329 ; So    [12] THERMOMETER..WIND BLOWING FACE
                             #x1F32A ; So    [12] THERMOMETER..WIND BLOWING FACE
                             #x1F32B ; So    [12] THERMOMETER..WIND BLOWING FACE
                             #x1F32C ; So    [12] THERMOMETER..WIND BLOWING FACE
                             #x1F32D ; So     [9] HOT DOG..CACTUS
                             #x1F32E ; So     [9] HOT DOG..CACTUS
                             #x1F32F ; So     [9] HOT DOG..CACTUS
                             #x1F330 ; So     [9] HOT DOG..CACTUS
                             #x1F331 ; So     [9] HOT DOG..CACTUS
                             #x1F332 ; So     [9] HOT DOG..CACTUS
                             #x1F333 ; So     [9] HOT DOG..CACTUS
                             #x1F334 ; So     [9] HOT DOG..CACTUS
                             #x1F335 ; So     [9] HOT DOG..CACTUS
                             #x1F336 ; So         HOT PEPPER
                             #x1F337 ; So    [70] TULIP..BABY BOTTLE
                             #x1F338 ; So    [70] TULIP..BABY BOTTLE
                             #x1F339 ; So    [70] TULIP..BABY BOTTLE
                             #x1F33A ; So    [70] TULIP..BABY BOTTLE
                             #x1F33B ; So    [70] TULIP..BABY BOTTLE
                             #x1F33C ; So    [70] TULIP..BABY BOTTLE
                             #x1F33D ; So    [70] TULIP..BABY BOTTLE
                             #x1F33E ; So    [70] TULIP..BABY BOTTLE
                             #x1F33F ; So    [70] TULIP..BABY BOTTLE
                             #x1F340 ; So    [70] TULIP..BABY BOTTLE
                             #x1F341 ; So    [70] TULIP..BABY BOTTLE
                             #x1F342 ; So    [70] TULIP..BABY BOTTLE
                             #x1F343 ; So    [70] TULIP..BABY BOTTLE
                             #x1F344 ; So    [70] TULIP..BABY BOTTLE
                             #x1F345 ; So    [70] TULIP..BABY BOTTLE
                             #x1F346 ; So    [70] TULIP..BABY BOTTLE
                             #x1F347 ; So    [70] TULIP..BABY BOTTLE
                             #x1F348 ; So    [70] TULIP..BABY BOTTLE
                             #x1F349 ; So    [70] TULIP..BABY BOTTLE
                             #x1F34A ; So    [70] TULIP..BABY BOTTLE
                             #x1F34B ; So    [70] TULIP..BABY BOTTLE
                             #x1F34C ; So    [70] TULIP..BABY BOTTLE
                             #x1F34D ; So    [70] TULIP..BABY BOTTLE
                             #x1F34E ; So    [70] TULIP..BABY BOTTLE
                             #x1F34F ; So    [70] TULIP..BABY BOTTLE
                             #x1F350 ; So    [70] TULIP..BABY BOTTLE
                             #x1F351 ; So    [70] TULIP..BABY BOTTLE
                             #x1F352 ; So    [70] TULIP..BABY BOTTLE
                             #x1F353 ; So    [70] TULIP..BABY BOTTLE
                             #x1F354 ; So    [70] TULIP..BABY BOTTLE
                             #x1F355 ; So    [70] TULIP..BABY BOTTLE
                             #x1F356 ; So    [70] TULIP..BABY BOTTLE
                             #x1F357 ; So    [70] TULIP..BABY BOTTLE
                             #x1F358 ; So    [70] TULIP..BABY BOTTLE
                             #x1F359 ; So    [70] TULIP..BABY BOTTLE
                             #x1F35A ; So    [70] TULIP..BABY BOTTLE
                             #x1F35B ; So    [70] TULIP..BABY BOTTLE
                             #x1F35C ; So    [70] TULIP..BABY BOTTLE
                             #x1F35D ; So    [70] TULIP..BABY BOTTLE
                             #x1F35E ; So    [70] TULIP..BABY BOTTLE
                             #x1F35F ; So    [70] TULIP..BABY BOTTLE
                             #x1F360 ; So    [70] TULIP..BABY BOTTLE
                             #x1F361 ; So    [70] TULIP..BABY BOTTLE
                             #x1F362 ; So    [70] TULIP..BABY BOTTLE
                             #x1F363 ; So    [70] TULIP..BABY BOTTLE
                             #x1F364 ; So    [70] TULIP..BABY BOTTLE
                             #x1F365 ; So    [70] TULIP..BABY BOTTLE
                             #x1F366 ; So    [70] TULIP..BABY BOTTLE
                             #x1F367 ; So    [70] TULIP..BABY BOTTLE
                             #x1F368 ; So    [70] TULIP..BABY BOTTLE
                             #x1F369 ; So    [70] TULIP..BABY BOTTLE
                             #x1F36A ; So    [70] TULIP..BABY BOTTLE
                             #x1F36B ; So    [70] TULIP..BABY BOTTLE
                             #x1F36C ; So    [70] TULIP..BABY BOTTLE
                             #x1F36D ; So    [70] TULIP..BABY BOTTLE
                             #x1F36E ; So    [70] TULIP..BABY BOTTLE
                             #x1F36F ; So    [70] TULIP..BABY BOTTLE
                             #x1F370 ; So    [70] TULIP..BABY BOTTLE
                             #x1F371 ; So    [70] TULIP..BABY BOTTLE
                             #x1F372 ; So    [70] TULIP..BABY BOTTLE
                             #x1F373 ; So    [70] TULIP..BABY BOTTLE
                             #x1F374 ; So    [70] TULIP..BABY BOTTLE
                             #x1F375 ; So    [70] TULIP..BABY BOTTLE
                             #x1F376 ; So    [70] TULIP..BABY BOTTLE
                             #x1F377 ; So    [70] TULIP..BABY BOTTLE
                             #x1F378 ; So    [70] TULIP..BABY BOTTLE
                             #x1F379 ; So    [70] TULIP..BABY BOTTLE
                             #x1F37A ; So    [70] TULIP..BABY BOTTLE
                             #x1F37B ; So    [70] TULIP..BABY BOTTLE
                             #x1F37C ; So    [70] TULIP..BABY BOTTLE
                             #x1F37D ; So         FORK AND KNIFE WITH PLATE
                             #x1F37E ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F37F ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F380 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F381 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F382 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F383 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F384 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F385 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F386 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F387 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F388 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F389 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F38A ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F38B ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F38C ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F38D ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F38E ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F38F ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F390 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F391 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F392 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F393 ; So    [22] BOTTLE WITH POPPING CORK..GRADUATION CAP
                             #x1F394 ; So    [12] HEART WITH TIP ON THE LEFT..ADMISSION TICKETS
                             #x1F395 ; So    [12] HEART WITH TIP ON THE LEFT..ADMISSION TICKETS
                             #x1F396 ; So    [12] HEART WITH TIP ON THE LEFT..ADMISSION TICKETS
                             #x1F397 ; So    [12] HEART WITH TIP ON THE LEFT..ADMISSION TICKETS
                             #x1F398 ; So    [12] HEART WITH TIP ON THE LEFT..ADMISSION TICKETS
                             #x1F399 ; So    [12] HEART WITH TIP ON THE LEFT..ADMISSION TICKETS
                             #x1F39A ; So    [12] HEART WITH TIP ON THE LEFT..ADMISSION TICKETS
                             #x1F39B ; So    [12] HEART WITH TIP ON THE LEFT..ADMISSION TICKETS
                             #x1F39C ; So    [12] HEART WITH TIP ON THE LEFT..ADMISSION TICKETS
                             #x1F39D ; So    [12] HEART WITH TIP ON THE LEFT..ADMISSION TICKETS
                             #x1F39E ; So    [12] HEART WITH TIP ON THE LEFT..ADMISSION TICKETS
                             #x1F39F ; So    [12] HEART WITH TIP ON THE LEFT..ADMISSION TICKETS
                             #x1F3A0 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3A1 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3A2 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3A3 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3A4 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3A5 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3A6 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3A7 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3A8 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3A9 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3AA ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3AB ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3AC ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3AD ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3AE ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3AF ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3B0 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3B1 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3B2 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3B3 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3B4 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3B5 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3B6 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3B7 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3B8 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3B9 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3BA ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3BB ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3BC ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3BD ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3BE ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3BF ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3C0 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3C1 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3C2 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3C3 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3C4 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3C5 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3C6 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3C7 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3C8 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3C9 ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3CA ; So    [43] CAROUSEL HORSE..SWIMMER
                             #x1F3CB ; So     [4] WEIGHT LIFTER..RACING CAR
                             #x1F3CC ; So     [4] WEIGHT LIFTER..RACING CAR
                             #x1F3CD ; So     [4] WEIGHT LIFTER..RACING CAR
                             #x1F3CE ; So     [4] WEIGHT LIFTER..RACING CAR
                             #x1F3CF ; So     [5] CRICKET BAT AND BALL..TABLE TENNIS PADDLE AND BALL
                             #x1F3D0 ; So     [5] CRICKET BAT AND BALL..TABLE TENNIS PADDLE AND BALL
                             #x1F3D1 ; So     [5] CRICKET BAT AND BALL..TABLE TENNIS PADDLE AND BALL
                             #x1F3D2 ; So     [5] CRICKET BAT AND BALL..TABLE TENNIS PADDLE AND BALL
                             #x1F3D3 ; So     [5] CRICKET BAT AND BALL..TABLE TENNIS PADDLE AND BALL
                             #x1F3D4 ; So    [12] SNOW CAPPED MOUNTAIN..STADIUM
                             #x1F3D5 ; So    [12] SNOW CAPPED MOUNTAIN..STADIUM
                             #x1F3D6 ; So    [12] SNOW CAPPED MOUNTAIN..STADIUM
                             #x1F3D7 ; So    [12] SNOW CAPPED MOUNTAIN..STADIUM
                             #x1F3D8 ; So    [12] SNOW CAPPED MOUNTAIN..STADIUM
                             #x1F3D9 ; So    [12] SNOW CAPPED MOUNTAIN..STADIUM
                             #x1F3DA ; So    [12] SNOW CAPPED MOUNTAIN..STADIUM
                             #x1F3DB ; So    [12] SNOW CAPPED MOUNTAIN..STADIUM
                             #x1F3DC ; So    [12] SNOW CAPPED MOUNTAIN..STADIUM
                             #x1F3DD ; So    [12] SNOW CAPPED MOUNTAIN..STADIUM
                             #x1F3DE ; So    [12] SNOW CAPPED MOUNTAIN..STADIUM
                             #x1F3DF ; So    [12] SNOW CAPPED MOUNTAIN..STADIUM
                             #x1F3E0 ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3E1 ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3E2 ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3E3 ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3E4 ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3E5 ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3E6 ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3E7 ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3E8 ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3E9 ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3EA ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3EB ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3EC ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3ED ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3EE ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3EF ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3F0 ; So    [17] HOUSE BUILDING..EUROPEAN CASTLE
                             #x1F3F1 ; So     [3] WHITE PENNANT..WAVING WHITE FLAG
                             #x1F3F2 ; So     [3] WHITE PENNANT..WAVING WHITE FLAG
                             #x1F3F3 ; So     [3] WHITE PENNANT..WAVING WHITE FLAG
                             #x1F3F4 ; So         WAVING BLACK FLAG
                             #x1F3F5 ; So     [3] ROSETTE..LABEL
                             #x1F3F6 ; So     [3] ROSETTE..LABEL
                             #x1F3F7 ; So     [3] ROSETTE..LABEL
                             #x1F3F8 ; So     [3] BADMINTON RACQUET AND SHUTTLECOCK..AMPHORA
                             #x1F3F9 ; So     [3] BADMINTON RACQUET AND SHUTTLECOCK..AMPHORA
                             #x1F3FA ; So     [3] BADMINTON RACQUET AND SHUTTLECOCK..AMPHORA
                             #x1F3FB ; Sk     [5] EMOJI MODIFIER FITZPATRICK TYPE-1-2..EMOJI MODIFIER FITZPATRICK TYPE-6
                             #x1F3FC ; Sk     [5] EMOJI MODIFIER FITZPATRICK TYPE-1-2..EMOJI MODIFIER FITZPATRICK TYPE-6
                             #x1F3FD ; Sk     [5] EMOJI MODIFIER FITZPATRICK TYPE-1-2..EMOJI MODIFIER FITZPATRICK TYPE-6
                             #x1F3FE ; Sk     [5] EMOJI MODIFIER FITZPATRICK TYPE-1-2..EMOJI MODIFIER FITZPATRICK TYPE-6
                             #x1F3FF ; Sk     [5] EMOJI MODIFIER FITZPATRICK TYPE-1-2..EMOJI MODIFIER FITZPATRICK TYPE-6
                             #x1F400 ; So    [63] RAT..PAW PRINTS
                             #x1F401 ; So    [63] RAT..PAW PRINTS
                             #x1F402 ; So    [63] RAT..PAW PRINTS
                             #x1F403 ; So    [63] RAT..PAW PRINTS
                             #x1F404 ; So    [63] RAT..PAW PRINTS
                             #x1F405 ; So    [63] RAT..PAW PRINTS
                             #x1F406 ; So    [63] RAT..PAW PRINTS
                             #x1F407 ; So    [63] RAT..PAW PRINTS
                             #x1F408 ; So    [63] RAT..PAW PRINTS
                             #x1F409 ; So    [63] RAT..PAW PRINTS
                             #x1F40A ; So    [63] RAT..PAW PRINTS
                             #x1F40B ; So    [63] RAT..PAW PRINTS
                             #x1F40C ; So    [63] RAT..PAW PRINTS
                             #x1F40D ; So    [63] RAT..PAW PRINTS
                             #x1F40E ; So    [63] RAT..PAW PRINTS
                             #x1F40F ; So    [63] RAT..PAW PRINTS
                             #x1F410 ; So    [63] RAT..PAW PRINTS
                             #x1F411 ; So    [63] RAT..PAW PRINTS
                             #x1F412 ; So    [63] RAT..PAW PRINTS
                             #x1F413 ; So    [63] RAT..PAW PRINTS
                             #x1F414 ; So    [63] RAT..PAW PRINTS
                             #x1F415 ; So    [63] RAT..PAW PRINTS
                             #x1F416 ; So    [63] RAT..PAW PRINTS
                             #x1F417 ; So    [63] RAT..PAW PRINTS
                             #x1F418 ; So    [63] RAT..PAW PRINTS
                             #x1F419 ; So    [63] RAT..PAW PRINTS
                             #x1F41A ; So    [63] RAT..PAW PRINTS
                             #x1F41B ; So    [63] RAT..PAW PRINTS
                             #x1F41C ; So    [63] RAT..PAW PRINTS
                             #x1F41D ; So    [63] RAT..PAW PRINTS
                             #x1F41E ; So    [63] RAT..PAW PRINTS
                             #x1F41F ; So    [63] RAT..PAW PRINTS
                             #x1F420 ; So    [63] RAT..PAW PRINTS
                             #x1F421 ; So    [63] RAT..PAW PRINTS
                             #x1F422 ; So    [63] RAT..PAW PRINTS
                             #x1F423 ; So    [63] RAT..PAW PRINTS
                             #x1F424 ; So    [63] RAT..PAW PRINTS
                             #x1F425 ; So    [63] RAT..PAW PRINTS
                             #x1F426 ; So    [63] RAT..PAW PRINTS
                             #x1F427 ; So    [63] RAT..PAW PRINTS
                             #x1F428 ; So    [63] RAT..PAW PRINTS
                             #x1F429 ; So    [63] RAT..PAW PRINTS
                             #x1F42A ; So    [63] RAT..PAW PRINTS
                             #x1F42B ; So    [63] RAT..PAW PRINTS
                             #x1F42C ; So    [63] RAT..PAW PRINTS
                             #x1F42D ; So    [63] RAT..PAW PRINTS
                             #x1F42E ; So    [63] RAT..PAW PRINTS
                             #x1F42F ; So    [63] RAT..PAW PRINTS
                             #x1F430 ; So    [63] RAT..PAW PRINTS
                             #x1F431 ; So    [63] RAT..PAW PRINTS
                             #x1F432 ; So    [63] RAT..PAW PRINTS
                             #x1F433 ; So    [63] RAT..PAW PRINTS
                             #x1F434 ; So    [63] RAT..PAW PRINTS
                             #x1F435 ; So    [63] RAT..PAW PRINTS
                             #x1F436 ; So    [63] RAT..PAW PRINTS
                             #x1F437 ; So    [63] RAT..PAW PRINTS
                             #x1F438 ; So    [63] RAT..PAW PRINTS
                             #x1F439 ; So    [63] RAT..PAW PRINTS
                             #x1F43A ; So    [63] RAT..PAW PRINTS
                             #x1F43B ; So    [63] RAT..PAW PRINTS
                             #x1F43C ; So    [63] RAT..PAW PRINTS
                             #x1F43D ; So    [63] RAT..PAW PRINTS
                             #x1F43E ; So    [63] RAT..PAW PRINTS
                             #x1F43F ; So         CHIPMUNK
                             #x1F440 ; So         EYES
                             #x1F441 ; So         EYE
                             #x1F442 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F443 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F444 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F445 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F446 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F447 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F448 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F449 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F44A ; So   [187] EAR..VIDEOCASSETTE
                             #x1F44B ; So   [187] EAR..VIDEOCASSETTE
                             #x1F44C ; So   [187] EAR..VIDEOCASSETTE
                             #x1F44D ; So   [187] EAR..VIDEOCASSETTE
                             #x1F44E ; So   [187] EAR..VIDEOCASSETTE
                             #x1F44F ; So   [187] EAR..VIDEOCASSETTE
                             #x1F450 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F451 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F452 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F453 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F454 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F455 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F456 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F457 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F458 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F459 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F45A ; So   [187] EAR..VIDEOCASSETTE
                             #x1F45B ; So   [187] EAR..VIDEOCASSETTE
                             #x1F45C ; So   [187] EAR..VIDEOCASSETTE
                             #x1F45D ; So   [187] EAR..VIDEOCASSETTE
                             #x1F45E ; So   [187] EAR..VIDEOCASSETTE
                             #x1F45F ; So   [187] EAR..VIDEOCASSETTE
                             #x1F460 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F461 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F462 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F463 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F464 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F465 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F466 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F467 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F468 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F469 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F46A ; So   [187] EAR..VIDEOCASSETTE
                             #x1F46B ; So   [187] EAR..VIDEOCASSETTE
                             #x1F46C ; So   [187] EAR..VIDEOCASSETTE
                             #x1F46D ; So   [187] EAR..VIDEOCASSETTE
                             #x1F46E ; So   [187] EAR..VIDEOCASSETTE
                             #x1F46F ; So   [187] EAR..VIDEOCASSETTE
                             #x1F470 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F471 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F472 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F473 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F474 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F475 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F476 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F477 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F478 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F479 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F47A ; So   [187] EAR..VIDEOCASSETTE
                             #x1F47B ; So   [187] EAR..VIDEOCASSETTE
                             #x1F47C ; So   [187] EAR..VIDEOCASSETTE
                             #x1F47D ; So   [187] EAR..VIDEOCASSETTE
                             #x1F47E ; So   [187] EAR..VIDEOCASSETTE
                             #x1F47F ; So   [187] EAR..VIDEOCASSETTE
                             #x1F480 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F481 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F482 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F483 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F484 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F485 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F486 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F487 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F488 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F489 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F48A ; So   [187] EAR..VIDEOCASSETTE
                             #x1F48B ; So   [187] EAR..VIDEOCASSETTE
                             #x1F48C ; So   [187] EAR..VIDEOCASSETTE
                             #x1F48D ; So   [187] EAR..VIDEOCASSETTE
                             #x1F48E ; So   [187] EAR..VIDEOCASSETTE
                             #x1F48F ; So   [187] EAR..VIDEOCASSETTE
                             #x1F490 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F491 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F492 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F493 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F494 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F495 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F496 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F497 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F498 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F499 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F49A ; So   [187] EAR..VIDEOCASSETTE
                             #x1F49B ; So   [187] EAR..VIDEOCASSETTE
                             #x1F49C ; So   [187] EAR..VIDEOCASSETTE
                             #x1F49D ; So   [187] EAR..VIDEOCASSETTE
                             #x1F49E ; So   [187] EAR..VIDEOCASSETTE
                             #x1F49F ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4A0 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4A1 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4A2 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4A3 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4A4 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4A5 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4A6 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4A7 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4A8 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4A9 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4AA ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4AB ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4AC ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4AD ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4AE ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4AF ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4B0 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4B1 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4B2 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4B3 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4B4 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4B5 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4B6 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4B7 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4B8 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4B9 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4BA ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4BB ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4BC ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4BD ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4BE ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4BF ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4C0 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4C1 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4C2 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4C3 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4C4 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4C5 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4C6 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4C7 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4C8 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4C9 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4CA ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4CB ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4CC ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4CD ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4CE ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4CF ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4D0 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4D1 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4D2 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4D3 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4D4 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4D5 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4D6 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4D7 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4D8 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4D9 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4DA ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4DB ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4DC ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4DD ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4DE ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4DF ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4E0 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4E1 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4E2 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4E3 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4E4 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4E5 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4E6 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4E7 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4E8 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4E9 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4EA ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4EB ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4EC ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4ED ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4EE ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4EF ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4F0 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4F1 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4F2 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4F3 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4F4 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4F5 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4F6 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4F7 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4F8 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4F9 ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4FA ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4FB ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4FC ; So   [187] EAR..VIDEOCASSETTE
                             #x1F4FD ; So     [2] FILM PROJECTOR..PORTABLE STEREO
                             #x1F4FE ; So     [2] FILM PROJECTOR..PORTABLE STEREO
                             #x1F4FF ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F500 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F501 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F502 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F503 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F504 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F505 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F506 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F507 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F508 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F509 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F50A ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F50B ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F50C ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F50D ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F50E ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F50F ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F510 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F511 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F512 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F513 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F514 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F515 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F516 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F517 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F518 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F519 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F51A ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F51B ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F51C ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F51D ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F51E ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F51F ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F520 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F521 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F522 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F523 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F524 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F525 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F526 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F527 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F528 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F529 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F52A ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F52B ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F52C ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F52D ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F52E ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F52F ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F530 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F531 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F532 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F533 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F534 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F535 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F536 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F537 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F538 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F539 ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F53A ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F53B ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F53C ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F53D ; So    [63] PRAYER BEADS..DOWN-POINTING SMALL RED TRIANGLE
                             #x1F53E ; So    [13] LOWER RIGHT SHADOWED WHITE CIRCLE..DOVE OF PEACE
                             #x1F53F ; So    [13] LOWER RIGHT SHADOWED WHITE CIRCLE..DOVE OF PEACE
                             #x1F540 ; So    [13] LOWER RIGHT SHADOWED WHITE CIRCLE..DOVE OF PEACE
                             #x1F541 ; So    [13] LOWER RIGHT SHADOWED WHITE CIRCLE..DOVE OF PEACE
                             #x1F542 ; So    [13] LOWER RIGHT SHADOWED WHITE CIRCLE..DOVE OF PEACE
                             #x1F543 ; So    [13] LOWER RIGHT SHADOWED WHITE CIRCLE..DOVE OF PEACE
                             #x1F544 ; So    [13] LOWER RIGHT SHADOWED WHITE CIRCLE..DOVE OF PEACE
                             #x1F545 ; So    [13] LOWER RIGHT SHADOWED WHITE CIRCLE..DOVE OF PEACE
                             #x1F546 ; So    [13] LOWER RIGHT SHADOWED WHITE CIRCLE..DOVE OF PEACE
                             #x1F547 ; So    [13] LOWER RIGHT SHADOWED WHITE CIRCLE..DOVE OF PEACE
                             #x1F548 ; So    [13] LOWER RIGHT SHADOWED WHITE CIRCLE..DOVE OF PEACE
                             #x1F549 ; So    [13] LOWER RIGHT SHADOWED WHITE CIRCLE..DOVE OF PEACE
                             #x1F54A ; So    [13] LOWER RIGHT SHADOWED WHITE CIRCLE..DOVE OF PEACE
                             #x1F54B ; So     [4] KAABA..MENORAH WITH NINE BRANCHES
                             #x1F54C ; So     [4] KAABA..MENORAH WITH NINE BRANCHES
                             #x1F54D ; So     [4] KAABA..MENORAH WITH NINE BRANCHES
                             #x1F54E ; So     [4] KAABA..MENORAH WITH NINE BRANCHES
                             #x1F54F ; So         BOWL OF HYGIEIA
                             #x1F550 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F551 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F552 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F553 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F554 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F555 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F556 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F557 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F558 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F559 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F55A ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F55B ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F55C ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F55D ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F55E ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F55F ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F560 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F561 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F562 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F563 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F564 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F565 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F566 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F567 ; So    [24] CLOCK FACE ONE OCLOCK..CLOCK FACE TWELVE-THIRTY
                             #x1F568 ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F569 ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F56A ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F56B ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F56C ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F56D ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F56E ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F56F ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F570 ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F571 ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F572 ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F573 ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F574 ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F575 ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F576 ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F577 ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F578 ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F579 ; So    [18] RIGHT SPEAKER..JOYSTICK
                             #x1F57A ; So         MAN DANCING
                             #x1F57B ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F57C ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F57D ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F57E ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F57F ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F580 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F581 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F582 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F583 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F584 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F585 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F586 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F587 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F588 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F589 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F58A ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F58B ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F58C ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F58D ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F58E ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F58F ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F590 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F591 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F592 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F593 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F594 ; So    [26] LEFT HAND TELEPHONE RECEIVER..REVERSED VICTORY HAND
                             #x1F595 ; So     [2] REVERSED HAND WITH MIDDLE FINGER EXTENDED..RAISED HAND WITH PART BETWEEN MIDDLE AND RING FINGERS
                             #x1F596 ; So     [2] REVERSED HAND WITH MIDDLE FINGER EXTENDED..RAISED HAND WITH PART BETWEEN MIDDLE AND RING FINGERS
                             #x1F597 ; So    [13] WHITE DOWN POINTING LEFT HAND INDEX..BLACK DOWN POINTING BACKHAND INDEX
                             #x1F598 ; So    [13] WHITE DOWN POINTING LEFT HAND INDEX..BLACK DOWN POINTING BACKHAND INDEX
                             #x1F599 ; So    [13] WHITE DOWN POINTING LEFT HAND INDEX..BLACK DOWN POINTING BACKHAND INDEX
                             #x1F59A ; So    [13] WHITE DOWN POINTING LEFT HAND INDEX..BLACK DOWN POINTING BACKHAND INDEX
                             #x1F59B ; So    [13] WHITE DOWN POINTING LEFT HAND INDEX..BLACK DOWN POINTING BACKHAND INDEX
                             #x1F59C ; So    [13] WHITE DOWN POINTING LEFT HAND INDEX..BLACK DOWN POINTING BACKHAND INDEX
                             #x1F59D ; So    [13] WHITE DOWN POINTING LEFT HAND INDEX..BLACK DOWN POINTING BACKHAND INDEX
                             #x1F59E ; So    [13] WHITE DOWN POINTING LEFT HAND INDEX..BLACK DOWN POINTING BACKHAND INDEX
                             #x1F59F ; So    [13] WHITE DOWN POINTING LEFT HAND INDEX..BLACK DOWN POINTING BACKHAND INDEX
                             #x1F5A0 ; So    [13] WHITE DOWN POINTING LEFT HAND INDEX..BLACK DOWN POINTING BACKHAND INDEX
                             #x1F5A1 ; So    [13] WHITE DOWN POINTING LEFT HAND INDEX..BLACK DOWN POINTING BACKHAND INDEX
                             #x1F5A2 ; So    [13] WHITE DOWN POINTING LEFT HAND INDEX..BLACK DOWN POINTING BACKHAND INDEX
                             #x1F5A3 ; So    [13] WHITE DOWN POINTING LEFT HAND INDEX..BLACK DOWN POINTING BACKHAND INDEX
                             #x1F5A4 ; So         BLACK HEART
                             #x1F5A5 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5A6 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5A7 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5A8 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5A9 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5AA ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5AB ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5AC ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5AD ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5AE ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5AF ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5B0 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5B1 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5B2 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5B3 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5B4 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5B5 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5B6 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5B7 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5B8 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5B9 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5BA ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5BB ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5BC ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5BD ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5BE ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5BF ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5C0 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5C1 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5C2 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5C3 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5C4 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5C5 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5C6 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5C7 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5C8 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5C9 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5CA ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5CB ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5CC ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5CD ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5CE ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5CF ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5D0 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5D1 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5D2 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5D3 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5D4 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5D5 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5D6 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5D7 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5D8 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5D9 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5DA ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5DB ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5DC ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5DD ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5DE ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5DF ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5E0 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5E1 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5E2 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5E3 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5E4 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5E5 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5E6 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5E7 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5E8 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5E9 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5EA ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5EB ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5EC ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5ED ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5EE ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5EF ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5F0 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5F1 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5F2 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5F3 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5F4 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5F5 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5F6 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5F7 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5F8 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5F9 ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5FA ; So    [86] DESKTOP COMPUTER..WORLD MAP
                             #x1F5FB ; So     [5] MOUNT FUJI..MOYAI
                             #x1F5FC ; So     [5] MOUNT FUJI..MOYAI
                             #x1F5FD ; So     [5] MOUNT FUJI..MOYAI
                             #x1F5FE ; So     [5] MOUNT FUJI..MOYAI
                             #x1F5FF ; So     [5] MOUNT FUJI..MOYAI
                             #x1F600 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F601 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F602 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F603 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F604 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F605 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F606 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F607 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F608 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F609 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F60A ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F60B ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F60C ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F60D ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F60E ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F60F ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F610 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F611 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F612 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F613 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F614 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F615 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F616 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F617 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F618 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F619 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F61A ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F61B ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F61C ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F61D ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F61E ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F61F ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F620 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F621 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F622 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F623 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F624 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F625 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F626 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F627 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F628 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F629 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F62A ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F62B ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F62C ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F62D ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F62E ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F62F ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F630 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F631 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F632 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F633 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F634 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F635 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F636 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F637 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F638 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F639 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F63A ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F63B ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F63C ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F63D ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F63E ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F63F ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F640 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F641 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F642 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F643 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F644 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F645 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F646 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F647 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F648 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F649 ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F64A ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F64B ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F64C ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F64D ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F64E ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F64F ; So    [80] GRINNING FACE..PERSON WITH FOLDED HANDS
                             #x1F650 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F651 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F652 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F653 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F654 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F655 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F656 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F657 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F658 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F659 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F65A ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F65B ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F65C ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F65D ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F65E ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F65F ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F660 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F661 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F662 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F663 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F664 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F665 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F666 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F667 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F668 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F669 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F66A ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F66B ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F66C ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F66D ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F66E ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F66F ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F670 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F671 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F672 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F673 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F674 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F675 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F676 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F677 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F678 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F679 ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F67A ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F67B ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F67C ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F67D ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F67E ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F67F ; So    [48] NORTH WEST POINTING LEAF..REVERSE CHECKER BOARD
                             #x1F680 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F681 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F682 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F683 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F684 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F685 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F686 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F687 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F688 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F689 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F68A ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F68B ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F68C ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F68D ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F68E ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F68F ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F690 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F691 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F692 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F693 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F694 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F695 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F696 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F697 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F698 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F699 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F69A ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F69B ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F69C ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F69D ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F69E ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F69F ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6A0 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6A1 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6A2 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6A3 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6A4 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6A5 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6A6 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6A7 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6A8 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6A9 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6AA ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6AB ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6AC ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6AD ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6AE ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6AF ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6B0 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6B1 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6B2 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6B3 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6B4 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6B5 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6B6 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6B7 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6B8 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6B9 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6BA ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6BB ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6BC ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6BD ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6BE ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6BF ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6C0 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6C1 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6C2 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6C3 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6C4 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6C5 ; So    [70] ROCKET..LEFT LUGGAGE
                             #x1F6C6 ; So     [6] TRIANGLE WITH ROUNDED CORNERS..COUCH AND LAMP
                             #x1F6C7 ; So     [6] TRIANGLE WITH ROUNDED CORNERS..COUCH AND LAMP
                             #x1F6C8 ; So     [6] TRIANGLE WITH ROUNDED CORNERS..COUCH AND LAMP
                             #x1F6C9 ; So     [6] TRIANGLE WITH ROUNDED CORNERS..COUCH AND LAMP
                             #x1F6CA ; So     [6] TRIANGLE WITH ROUNDED CORNERS..COUCH AND LAMP
                             #x1F6CB ; So     [6] TRIANGLE WITH ROUNDED CORNERS..COUCH AND LAMP
                             #x1F6CC ; So         SLEEPING ACCOMMODATION
                             #x1F6CD ; So     [3] SHOPPING BAGS..BED
                             #x1F6CE ; So     [3] SHOPPING BAGS..BED
                             #x1F6CF ; So     [3] SHOPPING BAGS..BED
                             #x1F6D0 ; So     [3] PLACE OF WORSHIP..SHOPPING TROLLEY
                             #x1F6D1 ; So     [3] PLACE OF WORSHIP..SHOPPING TROLLEY
                             #x1F6D2 ; So     [3] PLACE OF WORSHIP..SHOPPING TROLLEY
                             #x1F6D3 ; So     [2] STUPA..PAGODA
                             #x1F6D4 ; So     [2] STUPA..PAGODA
                             #x1F6D5 ; So     [3] HINDU TEMPLE..ELEVATOR
                             #x1F6D6 ; So     [3] HINDU TEMPLE..ELEVATOR
                             #x1F6D7 ; So     [3] HINDU TEMPLE..ELEVATOR
                             #x1F6E0 ; So    [11] HAMMER AND WRENCH..NORTHEAST-POINTING AIRPLANE
                             #x1F6E1 ; So    [11] HAMMER AND WRENCH..NORTHEAST-POINTING AIRPLANE
                             #x1F6E2 ; So    [11] HAMMER AND WRENCH..NORTHEAST-POINTING AIRPLANE
                             #x1F6E3 ; So    [11] HAMMER AND WRENCH..NORTHEAST-POINTING AIRPLANE
                             #x1F6E4 ; So    [11] HAMMER AND WRENCH..NORTHEAST-POINTING AIRPLANE
                             #x1F6E5 ; So    [11] HAMMER AND WRENCH..NORTHEAST-POINTING AIRPLANE
                             #x1F6E6 ; So    [11] HAMMER AND WRENCH..NORTHEAST-POINTING AIRPLANE
                             #x1F6E7 ; So    [11] HAMMER AND WRENCH..NORTHEAST-POINTING AIRPLANE
                             #x1F6E8 ; So    [11] HAMMER AND WRENCH..NORTHEAST-POINTING AIRPLANE
                             #x1F6E9 ; So    [11] HAMMER AND WRENCH..NORTHEAST-POINTING AIRPLANE
                             #x1F6EA ; So    [11] HAMMER AND WRENCH..NORTHEAST-POINTING AIRPLANE
                             #x1F6EB ; So     [2] AIRPLANE DEPARTURE..AIRPLANE ARRIVING
                             #x1F6EC ; So     [2] AIRPLANE DEPARTURE..AIRPLANE ARRIVING
                             #x1F6F0 ; So     [4] SATELLITE..PASSENGER SHIP
                             #x1F6F1 ; So     [4] SATELLITE..PASSENGER SHIP
                             #x1F6F2 ; So     [4] SATELLITE..PASSENGER SHIP
                             #x1F6F3 ; So     [4] SATELLITE..PASSENGER SHIP
                             #x1F6F4 ; So     [9] SCOOTER..ROLLER SKATE
                             #x1F6F5 ; So     [9] SCOOTER..ROLLER SKATE
                             #x1F6F6 ; So     [9] SCOOTER..ROLLER SKATE
                             #x1F6F7 ; So     [9] SCOOTER..ROLLER SKATE
                             #x1F6F8 ; So     [9] SCOOTER..ROLLER SKATE
                             #x1F6F9 ; So     [9] SCOOTER..ROLLER SKATE
                             #x1F6FA ; So     [9] SCOOTER..ROLLER SKATE
                             #x1F6FB ; So     [9] SCOOTER..ROLLER SKATE
                             #x1F6FC ; So     [9] SCOOTER..ROLLER SKATE
                             #x1F700 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F701 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F702 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F703 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F704 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F705 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F706 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F707 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F708 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F709 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F70A ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F70B ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F70C ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F70D ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F70E ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F70F ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F710 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F711 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F712 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F713 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F714 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F715 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F716 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F717 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F718 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F719 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F71A ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F71B ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F71C ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F71D ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F71E ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F71F ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F720 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F721 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F722 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F723 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F724 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F725 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F726 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F727 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F728 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F729 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F72A ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F72B ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F72C ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F72D ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F72E ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F72F ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F730 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F731 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F732 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F733 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F734 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F735 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F736 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F737 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F738 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F739 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F73A ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F73B ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F73C ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F73D ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F73E ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F73F ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F740 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F741 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F742 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F743 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F744 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F745 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F746 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F747 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F748 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F749 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F74A ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F74B ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F74C ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F74D ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F74E ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F74F ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F750 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F751 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F752 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F753 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F754 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F755 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F756 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F757 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F758 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F759 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F75A ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F75B ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F75C ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F75D ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F75E ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F75F ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F760 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F761 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F762 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F763 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F764 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F765 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F766 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F767 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F768 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F769 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F76A ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F76B ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F76C ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F76D ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F76E ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F76F ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F770 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F771 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F772 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F773 ; So   [116] ALCHEMICAL SYMBOL FOR QUINTESSENCE..ALCHEMICAL SYMBOL FOR HALF OUNCE
                             #x1F780 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F781 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F782 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F783 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F784 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F785 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F786 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F787 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F788 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F789 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F78A ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F78B ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F78C ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F78D ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F78E ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F78F ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F790 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F791 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F792 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F793 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F794 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F795 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F796 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F797 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F798 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F799 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F79A ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F79B ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F79C ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F79D ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F79E ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F79F ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7A0 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7A1 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7A2 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7A3 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7A4 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7A5 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7A6 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7A7 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7A8 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7A9 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7AA ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7AB ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7AC ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7AD ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7AE ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7AF ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7B0 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7B1 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7B2 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7B3 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7B4 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7B5 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7B6 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7B7 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7B8 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7B9 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7BA ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7BB ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7BC ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7BD ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7BE ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7BF ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7C0 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7C1 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7C2 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7C3 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7C4 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7C5 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7C6 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7C7 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7C8 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7C9 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7CA ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7CB ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7CC ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7CD ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7CE ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7CF ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7D0 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7D1 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7D2 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7D3 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7D4 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7D5 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7D6 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7D7 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7D8 ; So    [89] BLACK LEFT-POINTING ISOSCELES RIGHT TRIANGLE..NEGATIVE CIRCLED SQUARE
                             #x1F7E0 ; So    [12] LARGE ORANGE CIRCLE..LARGE BROWN SQUARE
                             #x1F7E1 ; So    [12] LARGE ORANGE CIRCLE..LARGE BROWN SQUARE
                             #x1F7E2 ; So    [12] LARGE ORANGE CIRCLE..LARGE BROWN SQUARE
                             #x1F7E3 ; So    [12] LARGE ORANGE CIRCLE..LARGE BROWN SQUARE
                             #x1F7E4 ; So    [12] LARGE ORANGE CIRCLE..LARGE BROWN SQUARE
                             #x1F7E5 ; So    [12] LARGE ORANGE CIRCLE..LARGE BROWN SQUARE
                             #x1F7E6 ; So    [12] LARGE ORANGE CIRCLE..LARGE BROWN SQUARE
                             #x1F7E7 ; So    [12] LARGE ORANGE CIRCLE..LARGE BROWN SQUARE
                             #x1F7E8 ; So    [12] LARGE ORANGE CIRCLE..LARGE BROWN SQUARE
                             #x1F7E9 ; So    [12] LARGE ORANGE CIRCLE..LARGE BROWN SQUARE
                             #x1F7EA ; So    [12] LARGE ORANGE CIRCLE..LARGE BROWN SQUARE
                             #x1F7EB ; So    [12] LARGE ORANGE CIRCLE..LARGE BROWN SQUARE
                             #x1F800 ; So    [12] LEFTWARDS ARROW WITH SMALL TRIANGLE ARROWHEAD..DOWNWARDS ARROW WITH LARGE TRIANGLE ARROWHEAD
                             #x1F801 ; So    [12] LEFTWARDS ARROW WITH SMALL TRIANGLE ARROWHEAD..DOWNWARDS ARROW WITH LARGE TRIANGLE ARROWHEAD
                             #x1F802 ; So    [12] LEFTWARDS ARROW WITH SMALL TRIANGLE ARROWHEAD..DOWNWARDS ARROW WITH LARGE TRIANGLE ARROWHEAD
                             #x1F803 ; So    [12] LEFTWARDS ARROW WITH SMALL TRIANGLE ARROWHEAD..DOWNWARDS ARROW WITH LARGE TRIANGLE ARROWHEAD
                             #x1F804 ; So    [12] LEFTWARDS ARROW WITH SMALL TRIANGLE ARROWHEAD..DOWNWARDS ARROW WITH LARGE TRIANGLE ARROWHEAD
                             #x1F805 ; So    [12] LEFTWARDS ARROW WITH SMALL TRIANGLE ARROWHEAD..DOWNWARDS ARROW WITH LARGE TRIANGLE ARROWHEAD
                             #x1F806 ; So    [12] LEFTWARDS ARROW WITH SMALL TRIANGLE ARROWHEAD..DOWNWARDS ARROW WITH LARGE TRIANGLE ARROWHEAD
                             #x1F807 ; So    [12] LEFTWARDS ARROW WITH SMALL TRIANGLE ARROWHEAD..DOWNWARDS ARROW WITH LARGE TRIANGLE ARROWHEAD
                             #x1F808 ; So    [12] LEFTWARDS ARROW WITH SMALL TRIANGLE ARROWHEAD..DOWNWARDS ARROW WITH LARGE TRIANGLE ARROWHEAD
                             #x1F809 ; So    [12] LEFTWARDS ARROW WITH SMALL TRIANGLE ARROWHEAD..DOWNWARDS ARROW WITH LARGE TRIANGLE ARROWHEAD
                             #x1F80A ; So    [12] LEFTWARDS ARROW WITH SMALL TRIANGLE ARROWHEAD..DOWNWARDS ARROW WITH LARGE TRIANGLE ARROWHEAD
                             #x1F80B ; So    [12] LEFTWARDS ARROW WITH SMALL TRIANGLE ARROWHEAD..DOWNWARDS ARROW WITH LARGE TRIANGLE ARROWHEAD
                             #x1F810 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F811 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F812 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F813 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F814 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F815 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F816 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F817 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F818 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F819 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F81A ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F81B ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F81C ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F81D ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F81E ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F81F ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F820 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F821 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F822 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F823 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F824 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F825 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F826 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F827 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F828 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F829 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F82A ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F82B ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F82C ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F82D ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F82E ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F82F ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F830 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F831 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F832 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F833 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F834 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F835 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F836 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F837 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F838 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F839 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F83A ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F83B ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F83C ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F83D ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F83E ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F83F ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F840 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F841 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F842 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F843 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F844 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F845 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F846 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F847 ; So    [56] LEFTWARDS ARROW WITH SMALL EQUILATERAL ARROWHEAD..DOWNWARDS HEAVY ARROW
                             #x1F850 ; So    [10] LEFTWARDS SANS-SERIF ARROW..UP DOWN SANS-SERIF ARROW
                             #x1F851 ; So    [10] LEFTWARDS SANS-SERIF ARROW..UP DOWN SANS-SERIF ARROW
                             #x1F852 ; So    [10] LEFTWARDS SANS-SERIF ARROW..UP DOWN SANS-SERIF ARROW
                             #x1F853 ; So    [10] LEFTWARDS SANS-SERIF ARROW..UP DOWN SANS-SERIF ARROW
                             #x1F854 ; So    [10] LEFTWARDS SANS-SERIF ARROW..UP DOWN SANS-SERIF ARROW
                             #x1F855 ; So    [10] LEFTWARDS SANS-SERIF ARROW..UP DOWN SANS-SERIF ARROW
                             #x1F856 ; So    [10] LEFTWARDS SANS-SERIF ARROW..UP DOWN SANS-SERIF ARROW
                             #x1F857 ; So    [10] LEFTWARDS SANS-SERIF ARROW..UP DOWN SANS-SERIF ARROW
                             #x1F858 ; So    [10] LEFTWARDS SANS-SERIF ARROW..UP DOWN SANS-SERIF ARROW
                             #x1F859 ; So    [10] LEFTWARDS SANS-SERIF ARROW..UP DOWN SANS-SERIF ARROW
                             #x1F860 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F861 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F862 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F863 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F864 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F865 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F866 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F867 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F868 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F869 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F86A ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F86B ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F86C ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F86D ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F86E ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F86F ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F870 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F871 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F872 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F873 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F874 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F875 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F876 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F877 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F878 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F879 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F87A ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F87B ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F87C ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F87D ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F87E ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F87F ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F880 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F881 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F882 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F883 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F884 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F885 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F886 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F887 ; So    [40] WIDE-HEADED LEFTWARDS LIGHT BARB ARROW..WIDE-HEADED SOUTH WEST VERY HEAVY BARB ARROW
                             #x1F890 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F891 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F892 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F893 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F894 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F895 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F896 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F897 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F898 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F899 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F89A ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F89B ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F89C ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F89D ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F89E ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F89F ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8A0 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8A1 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8A2 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8A3 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8A4 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8A5 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8A6 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8A7 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8A8 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8A9 ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8AA ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8AB ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8AC ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8AD ; So    [30] LEFTWARDS TRIANGLE ARROWHEAD..WHITE ARROW SHAFT WIDTH TWO THIRDS
                             #x1F8B0 ; So     [2] ARROW POINTING UPWARDS THEN NORTH WEST..ARROW POINTING RIGHTWARDS THEN CURVING SOUTH WEST
                             #x1F8B1 ; So     [2] ARROW POINTING UPWARDS THEN NORTH WEST..ARROW POINTING RIGHTWARDS THEN CURVING SOUTH WEST
                             #x1F900 ; So    [12] CIRCLED CROSS FORMEE WITH FOUR DOTS..DOWNWARD FACING NOTCHED HOOK WITH DOT
                             #x1F901 ; So    [12] CIRCLED CROSS FORMEE WITH FOUR DOTS..DOWNWARD FACING NOTCHED HOOK WITH DOT
                             #x1F902 ; So    [12] CIRCLED CROSS FORMEE WITH FOUR DOTS..DOWNWARD FACING NOTCHED HOOK WITH DOT
                             #x1F903 ; So    [12] CIRCLED CROSS FORMEE WITH FOUR DOTS..DOWNWARD FACING NOTCHED HOOK WITH DOT
                             #x1F904 ; So    [12] CIRCLED CROSS FORMEE WITH FOUR DOTS..DOWNWARD FACING NOTCHED HOOK WITH DOT
                             #x1F905 ; So    [12] CIRCLED CROSS FORMEE WITH FOUR DOTS..DOWNWARD FACING NOTCHED HOOK WITH DOT
                             #x1F906 ; So    [12] CIRCLED CROSS FORMEE WITH FOUR DOTS..DOWNWARD FACING NOTCHED HOOK WITH DOT
                             #x1F907 ; So    [12] CIRCLED CROSS FORMEE WITH FOUR DOTS..DOWNWARD FACING NOTCHED HOOK WITH DOT
                             #x1F908 ; So    [12] CIRCLED CROSS FORMEE WITH FOUR DOTS..DOWNWARD FACING NOTCHED HOOK WITH DOT
                             #x1F909 ; So    [12] CIRCLED CROSS FORMEE WITH FOUR DOTS..DOWNWARD FACING NOTCHED HOOK WITH DOT
                             #x1F90A ; So    [12] CIRCLED CROSS FORMEE WITH FOUR DOTS..DOWNWARD FACING NOTCHED HOOK WITH DOT
                             #x1F90B ; So    [12] CIRCLED CROSS FORMEE WITH FOUR DOTS..DOWNWARD FACING NOTCHED HOOK WITH DOT
                             #x1F90C ; So    [47] PINCHED FINGERS..FENCER
                             #x1F90D ; So    [47] PINCHED FINGERS..FENCER
                             #x1F90E ; So    [47] PINCHED FINGERS..FENCER
                             #x1F90F ; So    [47] PINCHED FINGERS..FENCER
                             #x1F910 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F911 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F912 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F913 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F914 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F915 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F916 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F917 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F918 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F919 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F91A ; So    [47] PINCHED FINGERS..FENCER
                             #x1F91B ; So    [47] PINCHED FINGERS..FENCER
                             #x1F91C ; So    [47] PINCHED FINGERS..FENCER
                             #x1F91D ; So    [47] PINCHED FINGERS..FENCER
                             #x1F91E ; So    [47] PINCHED FINGERS..FENCER
                             #x1F91F ; So    [47] PINCHED FINGERS..FENCER
                             #x1F920 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F921 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F922 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F923 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F924 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F925 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F926 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F927 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F928 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F929 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F92A ; So    [47] PINCHED FINGERS..FENCER
                             #x1F92B ; So    [47] PINCHED FINGERS..FENCER
                             #x1F92C ; So    [47] PINCHED FINGERS..FENCER
                             #x1F92D ; So    [47] PINCHED FINGERS..FENCER
                             #x1F92E ; So    [47] PINCHED FINGERS..FENCER
                             #x1F92F ; So    [47] PINCHED FINGERS..FENCER
                             #x1F930 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F931 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F932 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F933 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F934 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F935 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F936 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F937 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F938 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F939 ; So    [47] PINCHED FINGERS..FENCER
                             #x1F93A ; So    [47] PINCHED FINGERS..FENCER
                             #x1F93B ; So         MODERN PENTATHLON
                             #x1F93C ; So    [10] WRESTLERS..GOAL NET
                             #x1F93D ; So    [10] WRESTLERS..GOAL NET
                             #x1F93E ; So    [10] WRESTLERS..GOAL NET
                             #x1F93F ; So    [10] WRESTLERS..GOAL NET
                             #x1F940 ; So    [10] WRESTLERS..GOAL NET
                             #x1F941 ; So    [10] WRESTLERS..GOAL NET
                             #x1F942 ; So    [10] WRESTLERS..GOAL NET
                             #x1F943 ; So    [10] WRESTLERS..GOAL NET
                             #x1F944 ; So    [10] WRESTLERS..GOAL NET
                             #x1F945 ; So    [10] WRESTLERS..GOAL NET
                             #x1F946 ; So         RIFLE
                             #x1F947 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F948 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F949 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F94A ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F94B ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F94C ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F94D ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F94E ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F94F ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F950 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F951 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F952 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F953 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F954 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F955 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F956 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F957 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F958 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F959 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F95A ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F95B ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F95C ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F95D ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F95E ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F95F ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F960 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F961 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F962 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F963 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F964 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F965 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F966 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F967 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F968 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F969 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F96A ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F96B ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F96C ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F96D ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F96E ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F96F ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F970 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F971 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F972 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F973 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F974 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F975 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F976 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F977 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F978 ; So    [50] FIRST PLACE MEDAL..DISGUISED FACE
                             #x1F97A ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F97B ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F97C ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F97D ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F97E ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F97F ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F980 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F981 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F982 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F983 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F984 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F985 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F986 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F987 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F988 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F989 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F98A ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F98B ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F98C ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F98D ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F98E ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F98F ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F990 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F991 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F992 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F993 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F994 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F995 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F996 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F997 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F998 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F999 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F99A ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F99B ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F99C ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F99D ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F99E ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F99F ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9A0 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9A1 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9A2 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9A3 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9A4 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9A5 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9A6 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9A7 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9A8 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9A9 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9AA ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9AB ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9AC ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9AD ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9AE ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9AF ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9B0 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9B1 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9B2 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9B3 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9B4 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9B5 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9B6 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9B7 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9B8 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9B9 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9BA ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9BB ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9BC ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9BD ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9BE ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9BF ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9C0 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9C1 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9C2 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9C3 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9C4 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9C5 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9C6 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9C7 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9C8 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9C9 ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9CA ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9CB ; So    [82] FACE WITH PLEADING EYES..BUBBLE TEA
                             #x1F9CD ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9CE ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9CF ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9D0 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9D1 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9D2 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9D3 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9D4 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9D5 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9D6 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9D7 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9D8 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9D9 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9DA ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9DB ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9DC ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9DD ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9DE ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9DF ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9E0 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9E1 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9E2 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9E3 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9E4 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9E5 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9E6 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9E7 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9E8 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9E9 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9EA ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9EB ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9EC ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9ED ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9EE ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9EF ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9F0 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9F1 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9F2 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9F3 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9F4 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9F5 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9F6 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9F7 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9F8 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9F9 ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9FA ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9FB ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9FC ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9FD ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9FE ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1F9FF ; So    [51] STANDING PERSON..NAZAR AMULET
                             #x1FA00 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA01 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA02 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA03 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA04 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA05 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA06 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA07 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA08 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA09 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA0A ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA0B ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA0C ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA0D ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA0E ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA0F ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA10 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA11 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA12 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA13 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA14 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA15 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA16 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA17 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA18 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA19 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA1A ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA1B ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA1C ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA1D ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA1E ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA1F ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA20 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA21 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA22 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA23 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA24 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA25 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA26 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA27 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA28 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA29 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA2A ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA2B ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA2C ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA2D ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA2E ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA2F ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA30 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA31 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA32 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA33 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA34 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA35 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA36 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA37 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA38 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA39 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA3A ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA3B ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA3C ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA3D ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA3E ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA3F ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA40 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA41 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA42 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA43 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA44 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA45 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA46 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA47 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA48 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA49 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA4A ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA4B ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA4C ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA4D ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA4E ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA4F ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA50 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA51 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA52 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA53 ; So    [84] NEUTRAL CHESS KING..BLACK CHESS KNIGHT-BISHOP
                             #x1FA60 ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA61 ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA62 ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA63 ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA64 ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA65 ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA66 ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA67 ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA68 ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA69 ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA6A ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA6B ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA6C ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA6D ; So    [14] XIANGQI RED GENERAL..XIANGQI BLACK SOLDIER
                             #x1FA70 ; So     [5] BALLET SHOES..THONG SANDAL
                             #x1FA71 ; So     [5] BALLET SHOES..THONG SANDAL
                             #x1FA72 ; So     [5] BALLET SHOES..THONG SANDAL
                             #x1FA73 ; So     [5] BALLET SHOES..THONG SANDAL
                             #x1FA74 ; So     [5] BALLET SHOES..THONG SANDAL
                             #x1FA78 ; So     [3] DROP OF BLOOD..STETHOSCOPE
                             #x1FA79 ; So     [3] DROP OF BLOOD..STETHOSCOPE
                             #x1FA7A ; So     [3] DROP OF BLOOD..STETHOSCOPE
                             #x1FA80 ; So     [7] YO-YO..NESTING DOLLS
                             #x1FA81 ; So     [7] YO-YO..NESTING DOLLS
                             #x1FA82 ; So     [7] YO-YO..NESTING DOLLS
                             #x1FA83 ; So     [7] YO-YO..NESTING DOLLS
                             #x1FA84 ; So     [7] YO-YO..NESTING DOLLS
                             #x1FA85 ; So     [7] YO-YO..NESTING DOLLS
                             #x1FA86 ; So     [7] YO-YO..NESTING DOLLS
                             #x1FA90 ; So    [25] RINGED PLANET..ROCK
                             #x1FA91 ; So    [25] RINGED PLANET..ROCK
                             #x1FA92 ; So    [25] RINGED PLANET..ROCK
                             #x1FA93 ; So    [25] RINGED PLANET..ROCK
                             #x1FA94 ; So    [25] RINGED PLANET..ROCK
                             #x1FA95 ; So    [25] RINGED PLANET..ROCK
                             #x1FA96 ; So    [25] RINGED PLANET..ROCK
                             #x1FA97 ; So    [25] RINGED PLANET..ROCK
                             #x1FA98 ; So    [25] RINGED PLANET..ROCK
                             #x1FA99 ; So    [25] RINGED PLANET..ROCK
                             #x1FA9A ; So    [25] RINGED PLANET..ROCK
                             #x1FA9B ; So    [25] RINGED PLANET..ROCK
                             #x1FA9C ; So    [25] RINGED PLANET..ROCK
                             #x1FA9D ; So    [25] RINGED PLANET..ROCK
                             #x1FA9E ; So    [25] RINGED PLANET..ROCK
                             #x1FA9F ; So    [25] RINGED PLANET..ROCK
                             #x1FAA0 ; So    [25] RINGED PLANET..ROCK
                             #x1FAA1 ; So    [25] RINGED PLANET..ROCK
                             #x1FAA2 ; So    [25] RINGED PLANET..ROCK
                             #x1FAA3 ; So    [25] RINGED PLANET..ROCK
                             #x1FAA4 ; So    [25] RINGED PLANET..ROCK
                             #x1FAA5 ; So    [25] RINGED PLANET..ROCK
                             #x1FAA6 ; So    [25] RINGED PLANET..ROCK
                             #x1FAA7 ; So    [25] RINGED PLANET..ROCK
                             #x1FAA8 ; So    [25] RINGED PLANET..ROCK
                             #x1FAB0 ; So     [7] FLY..FEATHER
                             #x1FAB1 ; So     [7] FLY..FEATHER
                             #x1FAB2 ; So     [7] FLY..FEATHER
                             #x1FAB3 ; So     [7] FLY..FEATHER
                             #x1FAB4 ; So     [7] FLY..FEATHER
                             #x1FAB5 ; So     [7] FLY..FEATHER
                             #x1FAB6 ; So     [7] FLY..FEATHER
                             #x1FAC0 ; So     [3] ANATOMICAL HEART..PEOPLE HUGGING
                             #x1FAC1 ; So     [3] ANATOMICAL HEART..PEOPLE HUGGING
                             #x1FAC2 ; So     [3] ANATOMICAL HEART..PEOPLE HUGGING
                             #x1FAD0 ; So     [7] BLUEBERRIES..TEAPOT
                             #x1FAD1 ; So     [7] BLUEBERRIES..TEAPOT
                             #x1FAD2 ; So     [7] BLUEBERRIES..TEAPOT
                             #x1FAD3 ; So     [7] BLUEBERRIES..TEAPOT
                             #x1FAD4 ; So     [7] BLUEBERRIES..TEAPOT
                             #x1FAD5 ; So     [7] BLUEBERRIES..TEAPOT
                             #x1FAD6 ; So     [7] BLUEBERRIES..TEAPOT
                             #x1FB00 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB01 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB02 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB03 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB04 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB05 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB06 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB07 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB08 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB09 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB0A ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB0B ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB0C ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB0D ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB0E ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB0F ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB10 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB11 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB12 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB13 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB14 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB15 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB16 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB17 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB18 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB19 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB1A ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB1B ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB1C ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB1D ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB1E ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB1F ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB20 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB21 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB22 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB23 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB24 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB25 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB26 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB27 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB28 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB29 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB2A ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB2B ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB2C ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB2D ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB2E ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB2F ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB30 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB31 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB32 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB33 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB34 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB35 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB36 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB37 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB38 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB39 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB3A ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB3B ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB3C ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB3D ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB3E ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB3F ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB40 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB41 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB42 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB43 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB44 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB45 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB46 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB47 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB48 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB49 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB4A ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB4B ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB4C ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB4D ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB4E ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB4F ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB50 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB51 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB52 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB53 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB54 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB55 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB56 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB57 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB58 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB59 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB5A ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB5B ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB5C ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB5D ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB5E ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB5F ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB60 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB61 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB62 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB63 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB64 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB65 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB66 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB67 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB68 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB69 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB6A ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB6B ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB6C ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB6D ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB6E ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB6F ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB70 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB71 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB72 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB73 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB74 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB75 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB76 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB77 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB78 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB79 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB7A ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB7B ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB7C ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB7D ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB7E ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB7F ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB80 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB81 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB82 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB83 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB84 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB85 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB86 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB87 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB88 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB89 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB8A ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB8B ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB8C ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB8D ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB8E ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB8F ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB90 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB91 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB92 ; So   [147] BLOCK SEXTANT-1..UPPER HALF INVERSE MEDIUM SHADE AND LOWER HALF BLOCK
                             #x1FB94 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FB95 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FB96 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FB97 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FB98 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FB99 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FB9A ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FB9B ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FB9C ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FB9D ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FB9E ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FB9F ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBA0 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBA1 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBA2 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBA3 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBA4 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBA5 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBA6 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBA7 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBA8 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBA9 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBAA ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBAB ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBAC ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBAD ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBAE ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBAF ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBB0 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBB1 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBB2 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBB3 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBB4 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBB5 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBB6 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBB7 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBB8 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBB9 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBBA ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBBB ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBBC ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBBD ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBBE ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBBF ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBC0 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBC1 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBC2 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBC3 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBC4 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBC5 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBC6 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBC7 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBC8 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBC9 ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBCA ; So    [55] LEFT HALF INVERSE MEDIUM SHADE AND RIGHT HALF BLOCK..WHITE UP-POINTING CHEVRON
                             #x1FBF0 ; Nd    [10] SEGMENTED DIGIT ZERO..SEGMENTED DIGIT NINE
                             #x1FBF1 ; Nd    [10] SEGMENTED DIGIT ZERO..SEGMENTED DIGIT NINE
                             #x1FBF2 ; Nd    [10] SEGMENTED DIGIT ZERO..SEGMENTED DIGIT NINE
                             #x1FBF3 ; Nd    [10] SEGMENTED DIGIT ZERO..SEGMENTED DIGIT NINE
                             #x1FBF4 ; Nd    [10] SEGMENTED DIGIT ZERO..SEGMENTED DIGIT NINE
                             #x1FBF5 ; Nd    [10] SEGMENTED DIGIT ZERO..SEGMENTED DIGIT NINE
                             #x1FBF6 ; Nd    [10] SEGMENTED DIGIT ZERO..SEGMENTED DIGIT NINE
                             #x1FBF7 ; Nd    [10] SEGMENTED DIGIT ZERO..SEGMENTED DIGIT NINE
                             #x1FBF8 ; Nd    [10] SEGMENTED DIGIT ZERO..SEGMENTED DIGIT NINE
                             #x1FBF9 ; Nd    [10] SEGMENTED DIGIT ZERO..SEGMENTED DIGIT NINE
                             ))

(defun eaw-set-width (width)
  (when (= emacs-major-version 22)
    (utf-translate-cjk-set-unicode-range east-asian-ambiguous))
  (when (>= emacs-major-version 23)
    (while (char-table-parent char-width-table)
      (setq char-width-table (char-table-parent char-width-table)))
    (let ((table (make-char-table nil)))
      (mapc (lambda (range) (set-char-table-range table range width))
            east-asian-ambiguous)
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))

(defun eaw-fullwidth ()
  (eaw-set-width 2))

(provide 'eaw)

;;; eaw.el ends here

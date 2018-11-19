import Text.Regex

rgx = "(do|re|mi|fa|sol|la|si|[a-g]{1})(b|d|f|s|#)?([0-9]+)?(\\.)?"

checkNote str = matchRegex (mkRegex rgx) str

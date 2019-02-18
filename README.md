# regextools

To import the regex builder type

    import ai.lum.regextools.RegexBuilder

and to import the odin pattern builder

    import ai.lum.regextools.OdinPatternBuilder


To use the regex builder (or odin pattern builder)

    val b = new RegexBuilder
    b.add("foobar")
    b.add("fooxar")
    b.add("foozap")
    println(b.mkPattern)

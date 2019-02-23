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

You can visualize a diagram for the pattern by printing the
dot representation and plot it with graphviz (or http://www.webgraphviz.com/)

    println(b.mkDot)

If you want to import this, please type

    sbt publishLocal

and in your build.sbt add the dependency

    libraryDependencies += "ai.lum" %% "regextools" % "0.1.0-SNAPSHOT"

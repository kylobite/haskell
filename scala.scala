// scala.scala

println("Hello, World!")
/// "Hello, World!"

val msg0 = "I'm a value."
println(msg0)
/// "I'm a value."

val msg1: java.lang.String = "I'm a Java String."
println(msg1)
/// "I'm a Java String."

val msg2: String = "I'm also a Scala String."
println(msg2)
/// "I'm also a Scala String."

// msg0 = "Oh no!"
// error: reassignment to val

var msg3 = "I'm a variable."
msg3     = "I can change."
println(msg3)
/// "I can change."

val msg4 =
    | "Look ma, multiple lines!"
println(msg4)
/// "Look ma, multiple lines!"
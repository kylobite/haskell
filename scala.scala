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
    "Look ma, multiple lines!"
println(msg4)
/// "Look ma, multiple lines!"

def max(x: Int, y: Int): Int = {
    if (x > y) x
    else y
}
/// max: (x: Int,y: Int)Int

def max2(x: Int, y: Int) = if (x > y) x else y
/// max2: (x: Int,y: Int)Int

max(1, 2)
/// 2

def greet() = println("Hello!")
/// greet: ()Unit

if (args.length > 0) println("The first argument is: " + args(0))

var i = 0
while (i < args.length) {
    println(args(i))
    i += 1
}
/*
    arg0
    arg1
    arg2
 */

var i = 0;
while (i < args.length) {
    if (i != 0)
        print(" ")
    println(args(i))
    i += 1
}
println()
/// arg0 arg1 arg2



















































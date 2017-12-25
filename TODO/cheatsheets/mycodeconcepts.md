Roi's code concepts
=================

## Generics Variance, Covariance
  - consumer -> In
    ``` kotlin
    Comparable<in T>
    fun foo(numberComparable: Comparable<Number>) {
        val doubleComparable: Comparable<Double> = numberComparable
    }
    ```
    Use in means classes inheriting from T
    a Number is a like a double with less functionality but it has compare so we can use it in Comparable anyway
  - producer -> out
    ``` kotlin
    List<out T>
    val doubleList: List<Double> = listOf(1.0, 2.0)
    val numberList: List<Number> = doubleList
    ```
    Use out means super classes of T
    If we want List to be able to have inside it classes inheriting from T

  - Covariance means that the type F[B] is a subtype of the type F[A] if B is a subtype of A.
    ``` scala
    trait List[+A]
    trait Option[+A]
    
    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    val circles: List[Circle] = ???
    val shapes: List[Shape] = circle
  
  - contravariance means that the type F[B] is a subtype of F[A] if A is a subtype of B.
    ``` scala
    trait StringMe[-A] {
      def show(a: A): String
    }
    sealed trait Shape
    case class Circle(radius: Double) extends Shape
    val shape: Shape = Circle(1)
    val circle: Circle = Circle(100)
    val shapeCanString: StringMe[Shape] =
      new StringMe[Shape] {
        def show(shape: Shape) = s"Shape"
      }
    val circleCanString: StringMe[Circle] = shapeCanString
      // new StringMe[Circle] {
      //   def show(circle: Circle) = s"Shape + ${circle.radius}"
      // }
    def stringifier[A](value: A, stringer: StringMe[A]): String = stringer.show(value)

    stringifier(shape, shapeCanString) // Shape
    stringifier(circle, shapeCanString) // Shape
    //show2(shape, circleCanShow) // compiler error trait cant use a subtype of Shape for Shape
    stringifier(circle, circleCanString) // Shape // Shape + 100.0
    ```
    we can see we can show circle as a shape but not shape as a circle!
    meaning StringMe[Shape] is a subtype of StringMe[Circle]
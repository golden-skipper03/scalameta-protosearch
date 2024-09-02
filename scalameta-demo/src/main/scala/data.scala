  /** This is a Scaladoc comment
      * This is a description for the object Main.
      */
    object Main {
          /** This is a class representing a person
      * @param name The name of the person
      * @param age The age of the person
      */

      /** 
        * This function sums two integers.
        * @param a The first parameter
        * @param b The second parameter
        * @tparam T The type parameter 
        * For more information, visit [Sum Function](https://example.com/sum-function).
        */
      @deprecated("Use add instead", "1.0")
      def sum[T](a: Int, b: Int=1): Int = a + b

         /** 
        * This function greets the user.
        * @param name The name parameter
        * For more information, visit [Greet Function](https://example.com/greet-function).
        */
      @throws(classOf[IllegalArgumentException])  
      def greet(name: String): Unit = println(s"Hello, $name!")

      /** 
        * This function subtracts two integers.
        * @param c The first parameter to subtract
        * @param d The second parameter to subtract
        * @tparam T The type parameters
        */
      def subtraction[T](c: Int, d: Int): Int = c - d
    }
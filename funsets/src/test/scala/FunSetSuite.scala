package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
 

  test("intersection of {1,2} and {2,3} contains only 2 and not 1"){
    new TestSets {
      val set1 = union(s1, s2)
      val set2 = union(s2, s3)
      val intersection = intersect(set1, set2)
      assert(contains(intersection, 2), "Contains 2 ")
      assert(!contains(intersection, 1), "Does not contains 1")
    }
  }

  test("intersection between {1} and {2} is empty"){
    new TestSets {
      val emptySet = intersect(s1, s2)
      assert(!contains(emptySet, 1), "does not have 1")
      assert(!contains(emptySet, 2), "does not have 2")
    }
  }

  
  test("diff of {1,2} and {2,3} contains only 1 and not 2"){
    new TestSets {
      val set1 = union(s1, s2)
      val set2 = union(s2, s3)
      val difference = diff(set1, set2)
      assert(contains(difference, 1), "Contains 1")
      assert(!contains(difference, 2), "does not have 2")
    }
  }

  test("filter numbers > 1 from {1, 2} returns {2}"){
    new TestSets {
      val set = union(s1, s2)
      val filtered = filter(set, (x: Int) => x > 1)
      assert(contains(filtered, 2), "Contains 2")
      assert(!contains(filtered, 1), "does not have 1")
    }
  }

  test("all members of {1,2} have cubes less than 10"){
    new TestSets {
      val set = union(s1, s2)
      assert(forall(set, (x: Int) => x * x * x < 10), "all less than 10")
    }
  }


  test("there exists a number in {1,2} with value < 2"){
    new TestSets {
      val set = union(s1, s2)
      def check(x: Int) = x < 2 
      assert(exists(set, check), "all less than 10")
    }
  }
  
  test(" map({2}, x=>x^2) transforms it to {4}"){
    new TestSets {
      assert(contains(map(s2, (x: Int) => x * x ), 4), "contains 4")
    }
  }

}

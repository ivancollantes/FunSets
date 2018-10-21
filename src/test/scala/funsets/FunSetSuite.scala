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
   test("string take") {
     val message = "hello, world"
     assert(message.take(5) == "hello")
   }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
   test("adding ints") {
     assert(1 + 2 === 3)
   }


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
    val s12 = union(s1, s2) // 1, 2
    val s23 = union(s2, s3) // 2, 3

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
      assert(!contains(s1, 2), "Singleton")
      assert(!contains(s1, 3), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(!contains(s2, 1), "Singleton")
      assert(!contains(s2, 3), "Singleton")
      assert(contains(s3, 3), "Singleton")
      assert(!contains(s3, 1), "Singleton")
      assert(!contains(s3, 2), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      assert(contains(s12, 1), "Union 1")
      assert(contains(s12, 2), "Union 2")
      assert(!contains(s12, 3), "Union 3")
    }
  }

  test("intersect contains all elements in common of both set") {
    new TestSets {
      val s2only = intersect(s12, s23) // should be 2
      assert(contains(s2only, 2), "Intersect of union 12 and union 23 is 2")
      assert(!contains(s2only, 1), "Intersect of union 12 and union 23 is not 1")
      assert(!contains(s2only, 3), "Intersect of union 12 and union 23 is not 3")
    }
  }

  test("diff returns elements that are in one set but not in the other") {
    new TestSets {
      val s1only = diff(s12, s23) // should be 1
      assert(contains(s1only, 1), "Diff of union 12 and union 23 is 1")
      assert(!contains(s1only, 2), "Diff of union 12 and union 23 is not 2")
      assert(!contains(s1only, 3), "Diff of union 12 and union 23 is not 3")
    }
  }

  test("filter returns elements that are accepted by predicate p") {
    new TestSets {
      val s123 = union(s12, s23)
      assert(contains(filter(s123, x => x > 1), 3), "Union of 12 and 23 by 1 should return 3")
      assert(contains(filter(s123, x => x > 1), 2), "Union of 12 and 23 by 1 should not return 2")
      assert(!contains(filter(s123, x => x > 1), 1), "Union of 12 and 23 by 1 should not return 1")

      val s = union(singletonSet(1), union(singletonSet(3),
        union(singletonSet(4),
          union(singletonSet(5),
            union(singletonSet(7), singletonSet(500))))))

      assert(!contains(filter(s, x => x < 200), 500), "Does not contain 500")
      assert(contains(filter(s, x => x <= 500), 500), "Does contain 500")
    }
  }

  test("forall returns true if all elements in set satisfy the predicate p") {
    new TestSets {
      val s123 = union(s12, s23)
      val s = union(singletonSet(1), union(singletonSet(3),
                union(singletonSet(4),
                  union(singletonSet(5),
                    union(singletonSet(7), singletonSet(500))))))
      assert(forall(s123, x => x > 0), "All elements satisfy p")
      assert(!forall(s123, x => x > 1), "Not all elements satisfy p")
      assert(forall(s, x => x < 1000), "All elements satisfy p")
      assert(!forall(s, x => x < 5), "Not all elements satisfy p")
      assert(!forall(s, x => x == 2), "Not all elements satisfy p")
      assert(!forall(s, x => x % 2 == 1), "Not all elements satisfy p")
    }
  }

  test("exists returns true if at least one element in set satisfy the predicate p") {
    new TestSets {
      val s = union(singletonSet(1), union(singletonSet(3),
        union(singletonSet(4),
          union(singletonSet(5),
            union(singletonSet(7), singletonSet(500))))))
      assert(exists(s, x => x > 100), "At least one element satisfy p")
      assert(!forall(s, x => x > 1000), "None element satisfy p")
      assert(!forall(s, x => x < 0), "None element satisfy p")
      assert(!exists(s, x => x == 2), "At least one element satisfy p")
      assert(!exists(s, x => x % 2 == 1), "At least one element satisfy p")
    }
  }
}

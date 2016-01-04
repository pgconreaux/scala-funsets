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

  def negativeSet(): Set = { x => x < 0 }
  def positiveSet(): Set = { x => x > 0 }
  def allSet(): Set = { x => x == x }
  def emptySet(): Set = { x => false }

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

    val ns1 = singletonSet(-1)
    val ns2 = singletonSet(-2)
    val ns3 = singletonSet(-3)

    val neg = negativeSet()
    val pos = positiveSet()
    val all = allSet()
    val empty = emptySet()
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
      printSet(s1)
      assert(contains(s1, 1), "Singleton 1")
      printSet(s2)
      assert(contains(s2, 2), "Singleton 2")
      printSet(s3)
      assert(contains(s3, 3), "Singleton 3")
      assert(!contains(s3, 4), "Singleton is not 4")
      printSet(neg)
      assert(contains(neg, -1), "Negative contains -1")
      assert(contains(neg, -999), "Negative contains -999")
      assert(!contains(neg, 1), "Negative doesn't contain 1")
      assert(!contains(neg, 0), "Negative doesn't contain 0")
    }
  }

  test("union s1 and s2 contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      print("union(s1,s2): "); printSet(s)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("union s1 and ns1 contains all elements") {
    new TestSets {
      val s = union(s1, ns1)
      print("union(s1,ns1): "); printSet(s)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, -1), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intesect s1 and s2 contains only elements in both") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }

  test("intesect all and neg contains only elements in both") {
    new TestSets {
      val s = intersect(all, neg)
      println("intersect(all,neg): "); printSet(s)
      assert(!contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
      assert(contains(s, -1), "Intersect 4")
      assert(contains(s, -2), "Intersect 5")
      assert(contains(s, -3), "Intersect 6")
    }
  }

  test("intesect with negative contains only elements in both") {
    new TestSets {
      val neg1 = intersect(s1, neg)
      printSet(neg1)
      assert(!contains(neg1, 1), "Intersect Negative with 1")
      assert(!contains(neg1, -1), "Intersect Negative with -1")
      val neg2 = intersect(ns1, neg)
      printSet(neg2)
      assert(!contains(neg1, 1), "Intersect Negative with 1")
      assert(!contains(neg1, -1), "Intersect Negative with -1")
    }
  }

  test("diff contains difference of sets") {
    new TestSets {
      val s = diff(s1, s2)
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
      print("diff(all,pos): ");printSet(diff(all, pos))
      print("diff(pos,all): ");printSet(diff(pos,all))
    }
  }

  test("diff all and neg contains difference of sets") {
    new TestSets {
      val s = diff(all, neg)
      println("diff(all,neg): "); printSet(s)
      assert(contains(s, 0), "Diff 1")
      assert(contains(s, 1), "Diff 2")
      assert(!contains(s, -1), "Diff 3")
    }
  }

  test("filter all and neg returns subset that satisfies predicate") {
    new TestSets {
      val s = filter(all, neg)
      print("filter(all,neg): ");printSet(s)
      assert(contains(s, -1), "Filter 1")
      assert(!contains(s, 0), "Filter 2")
      assert(!contains(s, 1), "Filter 3")
    }
  }

  test("forall returns true iff all elements satisfy predicate") {
    new TestSets {
      assert(forall(neg, x => x < 0), "Forall 1")
      assert(!forall(neg, x => x > 0), "Forall 2")
      assert(forall(pos, x => x > 0), "Forall 2")
      assert(!forall(pos, x => x < 0), "Forall 3")
      assert(forall(s2, x => x > 0), "Forall 2")
      assert(forall(s2, x => x == 2), "Forall 2")
      assert(forall(empty, x => x == 2), "Forall 2")
    }
  }

  test("exists returns true iff at least one element satisfies predicate") {
    new TestSets {
      assert(exists(neg, x => x < 0), "Exists 1")
      assert(exists(s2, x => x == 2), "Exists 2")
      assert(exists(pos, x => x == 2), "Exists 3")
      assert(!exists(pos, x => x < 0), "Exists 4")
      assert(!exists(neg, x => x == 0), "Exists 5")
      assert(!exists(pos, x => x == 0), "Exists 6")
      assert(exists(all, x => x == 0), "Exists 7")
    }
  }

  test("map returns transformed set") {
    new TestSets {
      assert(!contains(map(neg, x => x * -1), 0), "Map 1")
      assert(!contains(map(neg, x => x * -1), -1), "Map 2")
      assert(contains(map(neg, x => x * -1), 1), "Map 3")
      assert(!contains(map(neg, x => x * -1), -1000), "Map 4")
      assert(contains(map(neg, x => x * -1), 1000), "Map 5")
      print("map(pos, x+1): ");printSet(map(pos, x => x + 1))
      assert(!contains(map(pos, x => x + 1), 1), "Map 6")
      assert(contains(map(pos, x => x + 1), 2), "Map 7")
      print("map(pos, x*2): ");printSet(map(pos, x => x * 2))
      assert(contains(map(pos, x => x * 2), 2), "Map 8")
      assert(!contains(map(pos, x => x * 2), 5), "Map 9")
      assert(contains(map(pos, x => x * 2), 6), "Map 10")
      
      print("s2: ");printSet(s2)
      println(s2(2))
      println(s2(3))
      print("map(s2, x+1): ");printSet(map(s2, x => x + 1))
      print("map(s2, x+1): ");printSet(map(s2, x => x * 2))
      assert(!contains(map(s2, x => x + 1), 2), "Map 6")
      assert(contains(map(s2, x => x + 1), 3), "Map 7")
    }
  }
}

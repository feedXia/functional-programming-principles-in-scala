package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

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

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

  /**
   * This test is currently disabled (by using .ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */

  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect contains elements of both sets") {
    new TestSets:
      val s = intersect(union(s1, s2), union(s2, s3))
      assert(!contains(s, 1), "Intersect 1")
      assert(contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
  }

  test("diff contains elements of first set not in second") {
    new TestSets:
      val s = diff(union(s1, s2), union(s2, s3))
      assert(contains(s, 1), "Diff 1")
      assert(!contains(s, 2), "Diff 2")
      assert(!contains(s, 3), "Diff 3")
  }

  test("filter contains subset where condition holds") {
    new TestSets:
      val s = filter(union(s1, s2), (x: Int) => x % 2 == 0)
      assert(!contains(s, 1), "Filter 1")
      assert(contains(s, 2), "Filter 2")
  }

  test("forall returns whether all elements satisfy condition") {
    new TestSets:
      val res1 = forall(union(s1, s3), (x: Int) => x % 2 == 1)
      val res2 = forall(union(s1, s2), (x: Int) => x % 2 == 1)
      assert(res1 == true , "Forall 1")
      assert(res2 == false , "Forall 2")
  }

  test("exists returns whether one element satisfies condition") {
    new TestSets:
      val res1 = exists(union(s1, s3), (x: Int) => x % 2 == 1)
      val res2 = exists(union(s1, s2), (x: Int) => x % 2 == 1)
      val res3 = exists(s2, (x: Int) => x % 2 == 1)
      assert(res1 == true, "Exists 1")
      assert(res2 == true, "Exists 2")
      assert(res3 == false, "Exists 3")
  }

  test("map transforms set by applying function to each element") {
    new TestSets:
      val s = map(union(s1, union(s2, s3)), (x: Int) => x * 2)
      assert(contains(s, 2), "Map 1")
      assert(contains(s, 4), "Map 2")
      assert(contains(s, 6), "Map 3")
      assert(!contains(s, 1), "Map 4")
      assert(!contains(s, 3), "Map 5")
  }



  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds

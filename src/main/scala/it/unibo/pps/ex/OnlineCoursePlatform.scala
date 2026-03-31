package it.unibo.pps.ex

import it.unibo.pps.util.Optionals.Optional
import it.unibo.pps.util.Sequences.*
import it.unibo.pps.util.Sequences.Sequence.Cons

import scala.annotation.tailrec // Assuming Sequence and related methods are here

// Represents a course offered on the platform
trait Course:
  def courseId: String // Unique identifier (e.g., "CS101", "SCALA01")
  def title: String
  def instructor: String
  def category: String // e.g., "Programming", "Data Science", "Design"

object Course:
  // Factory method for creating Course instances
  def apply(courseId: String, title: String, instructor: String, category: String): Course = CourseImpl(courseId, title, instructor, category)

  private case class CourseImpl(courseId: String, title: String, instructor: String, category: String) extends Course
/**
 * Manages courses and student enrollments on an online learning platform.
 */
trait OnlineCoursePlatform:
  /**
   * Adds a new course to the platform's catalog.
   * @param course The course to add.
   */
  def addCourse(course: Course): Unit

  /**
   * Finds courses belonging to a specific category.
   * @param category The category to search for.
   * @return A sequence of courses in that category.
   */
  def findCoursesByCategory(category: String): Sequence[Course]

  /**
   * Retrieves a specific course by its unique ID.
   * @param courseId The ID of the course to retrieve.
   * @return An Optional containing the course if found, otherwise Optional.empty.
   */
  def getCourse(courseId: String): Optional[Course]

  /**
   * Removes a course from the platform's catalog.
   * (Note: This basic version doesn't handle cascading removal of enrollments).
   * @param course The course to remove.
   */
  def removeCourse(course: Course): Unit

  /**
   * Checks if a course with the given ID exists in the catalog.
   * @param courseId The ID to check.
   * @return true if the course exists, false otherwise.
   */
  def isCourseAvailable(courseId: String): Boolean

  /**
   * Enrolls a student in a specific course.
   * Assumes studentId is unique for each student.
   * @param studentId The ID of the student.
   * @param courseId The ID of the course to enroll in.
   *                 Fails silently if the course doesn't exist.
   */
  def enrollStudent(studentId: String, courseId: String): Unit

  /**
   * Unenrolls a student from a specific course.
   * @param studentId The ID of the student.
   * @param courseId The ID of the course to unenroll from.
   */
  def unenrollStudent(studentId: String, courseId: String): Unit

  /**
   * Retrieves all courses a specific student is enrolled in.
   * @param studentId The ID of the student.
   * @return A sequence of courses the student is enrolled in.
   */
  def getStudentEnrollments(studentId: String): Sequence[Course]

  /**
   * Checks if a student is enrolled in a specific course.
   * @param studentId The ID of the student.
   * @param courseId The ID of the course.
   * @return true if the student is enrolled, false otherwise.
   */
  def isStudentEnrolled(studentId: String, courseId: String): Boolean

end OnlineCoursePlatform

object OnlineCoursePlatform:
  // Factory method for creating an empty platform instance
  def apply(): OnlineCoursePlatform = OnlineCoursePlatformImpl()

  private class OnlineCoursePlatformImpl extends OnlineCoursePlatform:
    private var courses: List[Course] = List()
    private var enrollments: Map[String, List[Course]] = Map()

    /**
     * Adds a new course to the platform's catalog.
     *
     * @param course The course to add.
     */
    override def addCourse(course: Course): Unit = courses = course :: courses

    /**
     * Finds courses belonging to a specific category.
     *
     * @param category The category to search for.
     * @return A sequence of courses in that category.
     */
    override def findCoursesByCategory(category: String): Sequence[Course] =
      def toSequence(list: List[Course]): Sequence[Course] = list match
        case head :: tail => Cons(head, toSequence(tail))
        case Nil => Sequence.Nil()

      toSequence(courses.filter(_.category == category))

    /**
     * Retrieves a specific course by its unique ID.
     *
     * @param courseId The ID of the course to retrieve.
     * @return An Optional containing the course if found, otherwise Optional.empty.
     */
    override def getCourse(courseId: String): Optional[Course] =
      @tailrec
      def getCourseById(currCourses: List[Course], courseId: String): Optional[Course] = currCourses match
        case head :: tail if head.courseId == courseId => Optional.Just(head)
        case head :: tail => getCourseById(tail, courseId)
        case _ => Optional.Empty()

      getCourseById(courses, courseId)

    /**
     * Removes a course from the platform's catalog.
     * (Note: This basic version doesn't handle cascading removal of enrollments).
     *
     * @param course The course to remove.
     */
    override def removeCourse(course: Course): Unit = courses = courses.filter(c => c != course)

    /**
     * Checks if a course with the given ID exists in the catalog.
     *
     * @param courseId The ID to check.
     * @return true if the course exists, false otherwise.
     */
    override def isCourseAvailable(courseId: String): Boolean = !courses.exists(c => c.courseId == courseId)

    /**
     * Enrolls a student in a specific course.
     * Assumes studentId is unique for each student.
     *
     * @param studentId The ID of the student.
     * @param courseId  The ID of the course to enroll in.
     *                  Fails silently if the course doesn't exist.
     */
    override def enrollStudent(studentId: String, courseId: String): Unit =
      val currentCourses = enrollments.getOrElse(studentId, List())

      courses.find(_.courseId == courseId) match
        case Some(course) =>
          enrollments =
            enrollments.updated(studentId, course :: currentCourses)
        case None =>
          ()

    /**
     * Unenrolls a student from a specific course.
     *
     * @param studentId The ID of the student.
     * @param courseId  The ID of the course to unenroll from.
     */
    override def unenrollStudent(studentId: String, courseId: String): Unit =
      val course = courses.find(_.courseId == courseId)
      course match
        case Some(c) =>
          val currentCourses = enrollments.getOrElse(studentId, List())
          enrollments =
            enrollments.updated(studentId, currentCourses.filterNot(_ == c))
        case None =>
          ()

    /**
     * Retrieves all courses a specific student is enrolled in.
     *
     * @param studentId The ID of the student.
     * @return A sequence of courses the student is enrolled in.
     */
    override def getStudentEnrollments(studentId: String): Sequence[Course] =
      def toSequence(list: List[Course]): Sequence[Course] = list match
        case head :: tail => Cons(head, toSequence(tail))
        case Nil => Sequence.Nil()

      toSequence(enrollments.getOrElse(studentId, List()))

    /**
     * Checks if a student is enrolled in a specific course.
     *
     * @param studentId The ID of the student.
     * @param courseId  The ID of the course.
     * @return true if the student is enrolled, false otherwise.
     */
    override def isStudentEnrolled(studentId: String, courseId: String): Boolean =
      enrollments.get(studentId) match
        case Some(courses) => courses.exists(_.courseId == courseId)
        case None => false


/**
 * Represents an online learning platform that offers courses and manages student enrollments.
 * Hints:
 * - Start by implementing the Course trait.
 *    - A case class might be a good fit for this.
 * - Implement the OnlineCoursePlatform trait.
 *    - Focus on how to represent the internal state
 *    - Two main entities: courses and student enrollments
 *    - Set for courses? List of enrollments?
 *  - Implement the factory method for creating an empty platform instance.
 *  - Now start incrementally following the main given
 *
 */
@main def mainPlatform(): Unit =
  val platform = OnlineCoursePlatform()

  val scalaCourse = Course("SCALA01", "Functional Programming in Scala", "Prof. Odersky", "Programming")
  val pythonCourse = Course("PYTHON01", "Introduction to Python", "Prof. van Rossum", "Programming")
  val designCourse = Course("DESIGN01", "UI/UX Design Fundamentals", "Prof. Norman", "Design")

  println(s"Is SCALA01 available? ${platform.isCourseAvailable(scalaCourse.courseId)}") // false
  platform.addCourse(scalaCourse)
  println(s"Is SCALA01 available? ${platform.isCourseAvailable(scalaCourse.courseId)}") // true
  platform.addCourse(pythonCourse)
  platform.addCourse(designCourse)

  println(s"Programming courses: ${platform.findCoursesByCategory("Programming")}") // Sequence(scalaCourse, pythonCourse)
  println(s"Design courses: ${platform.findCoursesByCategory("Design")}") // Sequence(designCourse)
  println(s"History courses: ${platform.findCoursesByCategory("History")}") // Sequence.empty

  println(s"Get SCALA01: ${platform.getCourse("SCALA01")}") // Optional.Just(scalaCourse)
  println(s"Get UNKNOWN01: ${platform.getCourse("UNKNOWN01")}") // Optional.Empty

  // Enrollments
  val studentAlice = "Alice123"
  val studentBob = "Bob456"

  println(s"Is Alice enrolled in SCALA01? ${platform.isStudentEnrolled(studentAlice, "SCALA01")}") // false
  platform.enrollStudent(studentAlice, "SCALA01")
  println(s"Is Alice enrolled in SCALA01? ${platform.isStudentEnrolled(studentAlice, "SCALA01")}") // true
  platform.enrollStudent(studentAlice, "DESIGN01")
  platform.enrollStudent(studentBob, "SCALA01") // Bob also enrolls in Scala

  println(s"Alice's enrollments: ${platform.getStudentEnrollments(studentAlice)}") // Sequence(scalaCourse, designCourse) - Order might vary
  println(s"Bob's enrollments: ${platform.getStudentEnrollments(studentBob)}") // Sequence(scalaCourse)

  platform.unenrollStudent(studentAlice, "SCALA01")
  println(s"Is Alice enrolled in SCALA01? ${platform.isStudentEnrolled(studentAlice, "SCALA01")}") // false
  println(s"Alice's enrollments: ${platform.getStudentEnrollments(studentAlice)}") // Sequence(designCourse)

  // Removal
  platform.removeCourse(pythonCourse)
  println(s"Is PYTHON01 available? ${platform.isCourseAvailable(pythonCourse.courseId)}") // false
  println(s"Programming courses: ${platform.findCoursesByCategory("Programming")}") // Sequence(scalaCourse)


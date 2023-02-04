import scala.reflect.ClassTag
object Extractor {
  def extract[T](list: List[Any])(implicit tag: ClassTag[T]) =
    list.flatMap {
      case element: T => Some(element)
      case _ => None
    }
}
val list: List[Any] = List(1, "string1", List(), "string2")
val result = Extractor.extract[String](list)
println(result) // List(string1, string2)
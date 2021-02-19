package map

object Task extends App {

  type Map[K, V] = K => Option[V]

  val map = emptyMap[String, Int]

  def emptyMap[K, V]: Map[K, V] = _ => None

  def put[K, V](m: Map[K, V], key: K, value: V): Map[K, V] = parameter =>
    if (parameter == key) Some(value)
    else m(parameter)

  val map2 = put(emptyMap[String, Int], "a", 1)
  val map3 = put(map2, "b", 2)

  println(map(""), map2("a"), map3("b"))

  val map4 = put(map3, "b", 3)

  println(map4(""), map4("a"), map4("b"))

  def delete[K, V](m: Map[K, V], key: K): Map[K, V] = parameter =>
    if (parameter == key) None
    else m(parameter)

  val map5 = delete(map4, "b")

  println(map5("b"), map5("a"))


  emptyMap[String, Int].apply("123")

  def get[K, V](m: Map[K, V], k: K): Option[V] = m.apply(k)
}

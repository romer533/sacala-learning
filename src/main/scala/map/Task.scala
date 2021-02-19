package main.scala.map

object Task extends App {

  type Map[K, V] = K => Option[V]

  def emptyMap[K, V]: Map[K, V] = ???

  def put[K, V](m: Map[K, V], key: K, value: V): Map[K, V] = ???

  def delete[K, V](m: Map[K, V], k: K): Map[K, V] = ???


  emptyMap[String, Int].apply("123")

  def get[K, V](m: Map[K, V], k: K): Option[V] = m.apply(k)
}

package exercises

object MyOption {

  def pure[A](value: A): Option[A] =
    Some(value)

  def flatMap[A, B](option: Option[A])(f: A => Option[B]): Option[B] =
    option.flatMap(f)

  def map[A, B](option: Option[A])(f: A => B): Option[B] = option.flatMap(value => pure(f(value)))
}

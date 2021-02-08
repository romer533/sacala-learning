package exercises.chapter3.ninth

import exercises.chapter3.MyList

object Length extends App {

  def length[A](as: MyList[A]): Int = MyList.foldRight(as, 0)((_, n) => n + 1)

  println(length(MyList(1, 2, 3, 4, 5)))

}

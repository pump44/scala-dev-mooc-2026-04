package ru.otus.module2

import ru.otus.module2.type_classes_hw.JsValue.{JsNull, JsNumber, JsObject, JsString}
import ru.otus.module2.type_classes_hw.JsonWriter.JsonWriterImpl

object type_classes_hw {


  // вернемся на scala 2
  // силд  ок
  sealed trait JsValue

  object JsValue {
    final case class JsObject(get: Map[String, JsValue]) extends JsValue  {
      override def toString: String = this.get.mkString("{\n", ", \n", "\n}")
    }

    final case class JsString(get: String) extends JsValue {
      override def toString: String = s""""${this.get}""""
    }


    final case class JsNumber(get: Double) extends JsValue {
      override def toString: String = s"""${this.get}"""
    }

    case object JsNull extends JsValue {
        override def toString: String = """null"""
    }
  }

  // 1

  trait JsonWriter[T] {
    def toJson(v: T): JsValue
  }
  
  object JsonWriter {
    
    def apply[T](implicit ev: JsonWriter[T]) = ev
    
    def from[T](f: T => JsValue): JsonWriter[T] = new JsonWriter[T] {
        override def toJson(v: T): JsValue = f(v)
    }

    // меняем все на имплиситы
    
    implicit val JsStringImpl: JsonWriter[String] = from[String](JsString)

    implicit val JsIntImpl:  JsonWriter[Int] = from[Int](JsNumber)

    // жуткий костыль, чтобы в мапе могли быть разные значения, ну естественно для которых есть JsonWriter
    implicit val JsAnyImpl: JsonWriter[Any] = from[Any] {
      case s: String => JsString(s)
      case n: Int => JsNumber(n)
      case opt: Option[Any] => toJson(opt)
      case _ => JsString("Not implemented")
    }


    implicit def  optJson [T](implicit jw: JsonWriter[T]): JsonWriter[Option[T]] = from[Option[T]] {
      case Some(value) => jw.toJson(value)
      case None => JsNull
    }

    implicit def JsObjImpl[T](implicit  jw: JsonWriter[T]): JsonWriter[Map[String, T]] =  from[Map[String, T]](
      m => JsObject(
        m.map {(k, v) => (k -> toJson(v))}
      )
    )

    // вот функции вида object.toJson видимо адо будет под каждый тип написать
    implicit class JsonWriterImpl(v: String) {
      def toJsonIm: JsValue = JsString(v)
    }

  }


  // T: JsonWriter кажется нормальная конструкция для scala 2, вроде такое было
  // ну или надо на имплиситли переписать
  // но у нас есть апплай с имплиситом
  def toJson[T: JsonWriter](v: T): JsValue = JsonWriter[T].toJson(v)



  @main
  def hw10(): Unit = {
    println(toJson("vffv"))
    println("vffv".toJsonIm)
    println(toJson(10))
    println(toJson(Option(10)))
    println(toJson(Option(List("").tail.headOption))) // идея не дает подсунуть просто None
    println(toJson(Map[String, Any](
      "one" -> 1,
      "two" -> "2",
      "opt" -> Option(22),
      "noImpl" -> 22.3
    )))
  }
}

















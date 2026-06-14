package ru.otus.module2

import ru.otus.module2.type_classes_hw.JsValue.{
  JsNull,
  JsNumber,
  JsObject,
  JsString
}
import ru.otus.module2.type_classes_hw.JsonWriter.given


object type_classes_hw {

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

    extension (v: T) def toJsonIm: JsValue
  }
  
  object JsonWriter {
    
    def apply[T](using ev: JsonWriter[T]) = ev
    
    def from[T](f: T => JsValue): JsonWriter[T] = new JsonWriter[T] {
        override def toJson(v: T): JsValue = f(v)

        extension (v: T) override def toJsonIm: JsValue = f(v)
    }
    
    given JsonWriter[String] = from[String](JsString)

    given JsonWriter[Int] = from[Int](JsNumber)


    given optJson [T](using jw: JsonWriter[T]): JsonWriter[Option[T]] = from[Option[T]] {
      case Some(value) => jw.toJson(value)
      case None => JsNull
    }

    given [T](using  jw: JsonWriter[T]): JsonWriter[Map[String, T]] =  from[Map[String, T]](
      m => JsObject(
        m.map {(k, v) => (k -> v.toJsonIm)}
      )
    )

  }


  def toJson[T: JsonWriter](v: T): JsValue = JsonWriter[T].toJson(v)
  


  @main
  def hw10(): Unit = {
    println(toJson("vffv"))
    println("vffv".toJsonIm)
    println(toJson(10))
    println(toJson(Option(10)))
    println(toJson(Option(List("").tail.headOption)))
    println(toJson(Map("one" -> Option("1"), "two" -> Option("2"))))
  }
}

















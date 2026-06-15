package ru.otus.module2

import ru.otus.module2.type_classes_hw.JsValue.{JsNull, JsNumber, JsObject, JsString}
import ru.otus.module2.type_classes_hw.{JsValue, JsonWriter}
import ru.otus.module2.type_classes_hw.JsonWriter.given


object type_classes_hw {

  // убрали скобки, вынесли toJson вне объекта потому что можем
  // выкинуть object type_classes_hw скорее всего тоже можем, но будут конфликты при импорте
  //в остальном вроде все вписывается вписывается в scala 3

  // меняем силд иерхию на enum
  enum JsValue:
    case JsObject(get: Map[String, JsValue])
    case JsString(get: String)
    case JsNumber(get: Double)
    case JsNull

    override def toString: String = this match {
      case JsValue.JsObject(get) => get.mkString("{\n", ", \n", "\n}")
      case JsValue.JsString(get) => s""""$get""""
      case JsValue.JsNumber(get) => s"$get"
      case JsValue.JsNull => "null"
    }
  
  trait JsonWriter[T]:
    def toJson(v: T): JsValue

    // по сути одно и тоже, просто по мне object.toJson красивее toJson(object)
    extension (v: T) def toJsonIm: JsValue

  
  object JsonWriter:
    
    def apply[T](using ev: JsonWriter[T]) = ev
    
    def from[T](f: T => JsValue): JsonWriter[T] = new JsonWriter[T] {
        override def toJson(v: T): JsValue = f(v)

        extension (v: T) override def toJsonIm: JsValue = f(v)
    }
    
    given JsonWriter[String] = from[String](JsString)

    given JsonWriter[Int] = from[Int](JsNumber)

    // жуткий костыль, чтобы в мапе могли быть разные значения, ну естественно для которых есть JsonWriter
    given JsonWriter[Any] = from[Any] {
      case s: String => JsString(s)
      case n: Int => JsNumber(n)
      case opt: Option[Any] => toJson(opt)
      case _ => JsString("Not implemented")
    }


    given optJson [T](using jw: JsonWriter[T]): JsonWriter[Option[T]] = from[Option[T]] {
      case Some(value) => jw.toJson(value)
      case None => JsNull
    }

    given [T]: JsonWriter[Map[String, T]] =  from[Map[String, T]](
      m => JsObject(
        m.map {(k, v) => (k -> v.toJsonIm)}
      )
    )

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

// может существовать без пакеджа и не завернут в другую структуру
def toJson[T: JsonWriter](v: T): JsValue = JsonWriter[T].toJson(v)
















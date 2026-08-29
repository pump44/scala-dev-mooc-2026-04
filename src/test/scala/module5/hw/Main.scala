package module5.hw

import java.time.Instant
import java.util.UUID


// ------------------------------------------------------------
// 1. Domain event: факт, который уже произошёл
// ------------------------------------------------------------

sealed trait CalculatorEvent {
  val calculationId: String
}

final case class CalculationCreated(
                                       calculationId: String,
                                       result: Double = 0.0
                                     ) extends CalculatorEvent

final case class CalculationPerformed(
                                       calculationId: String,
                                       operation: Operation,
                                       right: Double,
                                     ) extends CalculatorEvent

final case class CalculationError(
                                    calculationId: String,
                                    errorMessage: String
                                  ) extends CalculatorEvent


enum Operation:
    case Add
    case Subtract
    case Multiply
    case Divide
    case Place

// ------------------------------------------------------------
// 2. Envelope: техническая информация вокруг доменного события
// ------------------------------------------------------------

final case class EventEnvelope[E <: CalculatorEvent](
                                                  eventId: UUID,
                                                  aggregateId: String,
                                                  eventType: String,
                                                  version: Int,
                                                  occurredAt: Instant,
                                                  correlationId: Option[UUID],
                                                  causationId: Option[UUID],
                                                  producer: String,
                                                  payload: E
                                                )

// ------------------------------------------------------------
// 3. Event log: append-only история событий
// ------------------------------------------------------------

final class InMemoryEventLog:
  private var events: Vector[EventEnvelope[? <: CalculatorEvent]] =
    Vector.empty

  def append[E <: CalculatorEvent](event: EventEnvelope[E]): Unit =
    events = events :+ event

  def all: Vector[EventEnvelope[? <: CalculatorEvent]] =
    events

  def byAggregateId(
                     aggregateId: String
                   ): Vector[EventEnvelope[? <: CalculatorEvent]] =
    events.filter(_.aggregateId == aggregateId)

// ------------------------------------------------------------
// 4. Aggregate state
//
// Это НЕ CQRS read model.
// Это состояние aggregate, которое можно восстановить из событий.
// ------------------------------------------------------------


final case class CalculatorState(
                                  calculationId: String,
                                  errorMessage: String,
                                  result: BigDecimal
                           )

object Calculator:

  def evolve(
              state: Option[CalculatorState],
              event: CalculatorEvent
            ): Option[CalculatorState] =
    event match
      case e: CalculationCreated =>
        Some(
          CalculatorState(
            calculationId = e.calculationId,
            errorMessage = "",
            result = BigDecimal(e.result)
          )
        )
      case e: CalculationPerformed =>
        e.operation match {
          case Operation.Add =>
            Some(
              CalculatorState(
                calculationId = e.calculationId,
                errorMessage = "",
                result = state.map(_.result).getOrElse(BigDecimal(0)) + BigDecimal(e.right)
              )
            )
          case Operation.Subtract =>
              Some(
                CalculatorState(
                  calculationId = e.calculationId,
                  errorMessage = "",
                  result = state.map(_.result).getOrElse(BigDecimal(0)) - BigDecimal(e.right)
                )
              )
          case Operation.Multiply =>
              Some(
                CalculatorState(
                  calculationId = e.calculationId,
                  errorMessage = "",
                  result = state.map(_.result).getOrElse(BigDecimal(0))  * BigDecimal(e.right)
                )
              )
          case Operation.Divide =>
              if (e.right == 0) {
                Some(
                  CalculatorState(
                    calculationId = e.calculationId,
                    errorMessage = "Division by zero",
                    result = state.map(_.result).getOrElse(BigDecimal(0))
                  )
                )
              } else {
                Some(
                  CalculatorState(
                    calculationId = e.calculationId,
                    errorMessage = "",
                    result = state.map(_.result).getOrElse(BigDecimal(0)) / BigDecimal(e.right)
                  )
                )
              }
          case Operation.Place =>
              Some(
                CalculatorState(
                  calculationId = e.calculationId,
                  errorMessage = "",
                  result = BigDecimal(e.right)
                )
              )
        }

  def replay(
              events: Seq[EventEnvelope[? <: CalculatorEvent]]
            ): Option[CalculatorState] =
    events.foldLeft(Option.empty[CalculatorState]) {
      case (state, envelope) =>
        evolve(state, envelope.payload)
    }


// ------------------------------------------------------------
// 5. Demo
// ------------------------------------------------------------

@main def runEventExample(): Unit =

  val eventLog = new InMemoryEventLog

  val correlationId = UUID.randomUUID()

  val calculationId = "calculation-123"

  val calculationCreated =
    CalculationCreated(
      calculationId = calculationId
    )

  def envelope(e: CalculatorEvent) = {
    EventEnvelope(
      eventId = UUID.randomUUID(),
      aggregateId = e.calculationId,
      eventType = e.getClass.getName,
      version = 1,
      occurredAt = Instant.now(),
      correlationId = Some(correlationId),
      causationId = None,
      producer = "cal-service",
      payload = e
    )
  }

  val calculationPerformedPlace: CalculationPerformed = {
    CalculationPerformed(
      calculationId = calculationId,
      operation = Operation.Place,
      right = 100.0
    )
  }


  val calculationPerformedMultiply: CalculationPerformed = {
       CalculationPerformed(
         calculationId = calculationId,
         operation = Operation.Multiply,
         right = 4.4
       )
  }

  val calculationPerformedDivide: CalculationPerformed = {
    CalculationPerformed(
      calculationId = calculationId,
      operation = Operation.Divide,
      right = 0
    )
  }

  // событие только добавляется в log
  eventLog.append(envelope(calculationCreated))
  eventLog.append(envelope(calculationPerformedPlace))
  eventLog.append(envelope(calculationPerformedMultiply))
  eventLog.append(envelope(calculationPerformedMultiply))
  eventLog.append(envelope(calculationPerformedMultiply))
  eventLog.append(envelope(calculationPerformedDivide))

  println("=== Event log ===")

  eventLog.all.zipWithIndex.foreach {
    case (event, offset) =>
      println(
        s"offset=$offset, " +
          s"type=${event.eventType}, " +
          s"aggregateId=${event.aggregateId}, " +
          s"payload=${event.payload}"
      )
  }

  println()
  println("=== Replay aggregate state ===")

  val orderEvents =
    eventLog.byAggregateId(calculationId)

  val state =
    Calculator.replay(orderEvents)

  println(state)

  println()
  println("=== Important idea ===")
  println("Domain event = факт")
  println("Event log = история")
  println("Replay = восстановление aggregate state")
  println("CQRS projection/read model появится позже")

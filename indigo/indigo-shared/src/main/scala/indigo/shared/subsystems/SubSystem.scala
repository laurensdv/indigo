package indigo.shared.subsystems

import indigo.shared.Outcome
import indigo.shared.events.GlobalEvent
import indigo.shared.scenegraph.SceneUpdateFragment

trait SubSystem {
  type EventType
  type SubSystemModel

  def eventFilter: GlobalEvent => Option[EventType]

  def initialModel: Outcome[SubSystemModel]

  def update(context: SubSystemFrameContext, model: SubSystemModel): EventType => Outcome[SubSystemModel]

  def present(context: SubSystemFrameContext, model: SubSystemModel): Outcome[SceneUpdateFragment]
}

object SubSystem {

  def apply[Event, Model](
      _eventFilter: GlobalEvent => Option[Event],
      _initialModel: Outcome[Model],
      _update: (SubSystemFrameContext, Model) => Event => Outcome[Model],
      _present: (SubSystemFrameContext, Model) => Outcome[SceneUpdateFragment]
  ): SubSystem =
    new SubSystem {
      type EventType      = Event
      type SubSystemModel = Model

      def eventFilter: GlobalEvent => Option[EventType] =
        _eventFilter

      def initialModel: Outcome[SubSystemModel] =
        _initialModel

      def update(context: SubSystemFrameContext, model: SubSystemModel): EventType => Outcome[SubSystemModel] =
        _update(context, model)

      def present(context: SubSystemFrameContext, model: SubSystemModel): Outcome[SceneUpdateFragment] =
        _present(context, model)
    }

}

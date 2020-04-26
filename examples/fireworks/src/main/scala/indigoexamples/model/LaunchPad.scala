package indigoexamples.model

import indigo._
import indigoexts.subsystems.automata.AutomatonPayload
import indigoexamples.automata.LaunchPadAutomata
import indigoexts.geometry.Vertex

final case class LaunchPad(position: Vertex, countDown: Seconds, rocket: Rocket) extends AutomatonPayload

object LaunchPad {

  def generateLaunchPad(dice: Dice): LaunchPad = {
    val startPosition: Vertex =
      Vertex((dice.rollDouble * 2) - 1.0d, 0)

    val diff: Seconds =
      Millis(
        dice
          .roll(
            (LaunchPadAutomata.MaxCountDown - LaunchPadAutomata.MinCountDown).toMillis.value.toInt
          )
          .toLong
      ).toSeconds

    val countDown: Seconds =
      LaunchPadAutomata.MinCountDown + diff

    LaunchPad(startPosition, countDown, Rocket.generateRocket(dice, startPosition))
  }

}

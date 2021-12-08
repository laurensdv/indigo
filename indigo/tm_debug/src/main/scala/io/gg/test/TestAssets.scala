package io.gg.test

import indigo.*

import scala.language.postfixOps

object TestAssets {

  val spritesheet: AssetName   = AssetName("spritesheet")
  val tilemap: AssetName = AssetName("tiledmap")

  def assets: Set[AssetType] =
    Set(
      AssetType.Image(spritesheet, AssetPath("assets/" + spritesheet + ".png")),
      AssetType.Text(tilemap, AssetPath("assets/" + tilemap + ".json"))
    )

}


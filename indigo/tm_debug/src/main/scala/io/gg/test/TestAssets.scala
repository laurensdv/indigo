package io.gg.test

import indigo._

object TestAssets {

  val spritesheet: AssetName   = AssetName("spritesheet")

  def assets: Set[AssetType] =
    Set(
      AssetType.Image(spritesheet, AssetPath("assets/" + spritesheet + ".png"))
    )

}


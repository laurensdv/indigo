package indigo.shared

import indigo.shared

trait SharedTypeAliases {

  type AssetType = shared.assets.AssetType
  val AssetType: shared.assets.AssetType.type = shared.assets.AssetType

  type GameConfig = shared.config.GameConfig
  val GameConfig: shared.config.GameConfig.type = shared.config.GameConfig

  type GameViewport = shared.config.GameViewport
  val GameViewport: shared.config.GameViewport.type = shared.config.GameViewport

  type AdvancedGameConfig = shared.config.AdvancedGameConfig
  val AdvancedGameConfig: shared.config.AdvancedGameConfig.type = shared.config.AdvancedGameConfig

  type RenderingTechnology = shared.config.RenderingTechnology
  val RenderingTechnology: shared.config.RenderingTechnology.type = shared.config.RenderingTechnology

  val IndigoLogger: shared.IndigoLogger.type = shared.IndigoLogger

  type Aseprite = shared.formats.Aseprite
  val Aseprite: shared.formats.Aseprite.type = shared.formats.Aseprite

  type SpriteAndAnimations = shared.formats.SpriteAndAnimations
  val SpriteAndAnimations: shared.formats.SpriteAndAnimations.type = shared.formats.SpriteAndAnimations

  type TiledMap = shared.formats.TiledMap
  val TiledMap: shared.formats.TiledMap.type = shared.formats.TiledMap

  type TiledGridMap[A] = shared.formats.TiledGridMap[A]
  val TiledGridMap: shared.formats.TiledGridMap.type = shared.formats.TiledGridMap

  type TiledGridLayer[A] = shared.formats.TiledGridLayer[A]
  val TiledGridLayer: shared.formats.TiledGridLayer.type = shared.formats.TiledGridLayer

  type TiledGridCell[A] = shared.formats.TiledGridCell[A]
  val TiledGridCell: shared.formats.TiledGridCell.type = shared.formats.TiledGridCell

  type Gamepad = shared.input.Gamepad
  val Gamepad: shared.input.Gamepad.type = shared.input.Gamepad

  type GamepadDPad = shared.input.GamepadDPad
  val GamepadDPad: shared.input.GamepadDPad.type = shared.input.GamepadDPad

  type GamepadAnalogControls = shared.input.GamepadAnalogControls
  val GamepadAnalogControls: shared.input.GamepadAnalogControls.type = shared.input.GamepadAnalogControls

  type AnalogAxis = shared.input.AnalogAxis
  val AnalogAxis: shared.input.AnalogAxis.type = shared.input.AnalogAxis

  type GamepadButtons = shared.input.GamepadButtons
  val GamepadButtons: shared.input.GamepadButtons.type = shared.input.GamepadButtons

  type BoundaryLocator = shared.BoundaryLocator

  type FrameContext[StartUpData] = shared.FrameContext[StartUpData]
  type SubSystemFrameContext     = shared.subsystems.SubSystemFrameContext

}

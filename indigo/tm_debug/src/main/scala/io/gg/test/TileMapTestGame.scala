package io.gg.test

import indigo._
import indigo.json.Json
import indigoextras.subsystems.FPSCounter
import scala.scalajs.js.annotation._
import io.gg.test.{TestAssets, TestResource}

@JSExportTopLevel("IndigoGame")
object TileMapTestGame extends IndigoDemo[Unit, TileMapContext, Unit, Unit] {

  val targetFPS: Int          = 60
  val viewportWidth: Int      = 800
  val viewportHeight: Int     = 600
  val magnificationLevel: Int = 2

  val eventFilters: EventFilters =
    EventFilters(
      {
        case e: FrameTick =>
          Some(e)

        case e: KeyboardEvent.KeyDown =>
          Some(e)

        case e: KeyboardEvent.KeyUp =>
          Some(e)

        case _ =>
          None
      },
      _ => None
    )

  def boot(flags: Map[String, String]): Outcome[BootResult[Unit]] =
    Outcome {
      BootResult
        .noData(
          GameConfig(
            viewport = GameViewport(viewportWidth, viewportHeight),
            frameRate = targetFPS,
            clearColor = RGBA(0.4, 0.2, 0.5, 1),
            magnification = magnificationLevel,
            advanced = AdvancedGameConfig(
              renderingTechnology = RenderingTechnology.WebGL2,
              antiAliasing = false,
              batchSize = 512,
              disableSkipModelUpdates = true,
              disableSkipViewUpdates = true,
              autoLoadStandardShaders = false
            )
          )
        )
        .withAssets(TestAssets.assets)
        .withSubSystems(FPSCounter(Point(10, 565), targetFPS, None))
        .withShaders(
          StandardShaders.Bitmap,
          StandardShaders.ImageEffects,
          StandardShaders.NormalBlend,
          StandardShaders.ShapeBox
        )
    }

  def initialModel(startupData: TileMapContext): Outcome[Unit] =
    Outcome(())

  def initialViewModel(startupData: TileMapContext, model: Unit): Outcome[Unit] =
    Outcome(())

  def setup(bootData: Unit, assetCollection: AssetCollection, dice: Dice): Outcome[Startup[TileMapContext]] = {
    val maybeTiledMap = for {
          t <- Json.tiledMapFromJson(TestResource.tileMap)
        } yield t

    val maybeTiledMapCloneGroup: Option[(List[CloneBlank], Group)] = for {
      t <- maybeTiledMap
      g <- t.toCLoneGroup(TestAssets.spritesheet) // read image from tileset json
    } yield g

    val maybeAdditionalAnimations: Option[Seq[Iterable[Animation]]] = for {
      t <- maybeTiledMap
      a <- t.parseAnimations()
    } yield a

    val res: (List[CloneBlank], Group)  = maybeTiledMapCloneGroup.getOrElse((List.empty, Group.empty))
    
    maybeAdditionalAnimations match {
      case None => Outcome(Startup.Success(TileMapContext(res._2, res._1)))
      case Some(animations) => Outcome(Startup.Success(TileMapContext(res._2, res._1)).addAnimations(animations.flatten.toList))
    }
  }

  def updateModel(context: FrameContext[TileMapContext], model: Unit): GlobalEvent => Outcome[Unit] = {
    _ => Outcome(model)
  }

  def updateViewModel(context: FrameContext[TileMapContext], model: Unit, viewModel: Unit): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(context: FrameContext[TileMapContext], model: Unit, viewModel: Unit): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty.addCloneBlanks(context.startUpData.cloneBlanks).addLayer(List(context.startUpData.group)))

}

final case class TileMapContext(group: Group = Group.empty,
                                cloneBlanks: List[CloneBlank] = List.empty)

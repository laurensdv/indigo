package indigo.shared.formats

import indigo.shared.animation.Animation
import indigo.shared.animation.AnimationKey
import indigo.shared.animation.Frame
import indigo.shared.assets.AssetName
import indigo.shared.collections._
import indigo.shared.datatypes.BindingKey
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Radians
import indigo.shared.datatypes.Rectangle
import indigo.shared.datatypes.Size
import indigo.shared.materials.Material
import indigo.shared.scenegraph.CloneBatch
import indigo.shared.scenegraph.CloneBatchData
import indigo.shared.scenegraph.CloneBlank
import indigo.shared.scenegraph.CloneId
import indigo.shared.scenegraph.CloneTileData
import indigo.shared.scenegraph.CloneTiles
import indigo.shared.scenegraph.Graphic
import indigo.shared.scenegraph.Group
import indigo.shared.scenegraph.SceneNode
import indigo.shared.scenegraph.Sprite
import indigo.shared.time.Millis

import scala.annotation.tailrec

/*
Full spec is here:
http://doc.mapeditor.org/reference/tmx-map-format/

This is not a full implementation. No doubt I'll be adding and tweaking as I go based on requirements.
 */
 
final case class TiledMap(
                           width: Int,
                           height: Int,
                           infinite: Boolean,
                           layers: List[TiledLayer],
                           nextobjectid: Int,   // Stores the next available ID for new objects. This number is stored to prevent reuse of the same ID after objects have been removed.
                           orientation: String, // orthogonal, isometric, staggered and hexagonal
                           renderorder: String, // right-down (the default), right-up, left-down and left-up. In all cases, the map is drawn row-by-row.
                           tiledversion: String,
                           tilewidth: Int,
                           tileheight: Int,
                           tilesets: List[TileSet],
                           `type`: String, // "map"
                           hexsidelength: Option[Int],
                           staggeraxis: Option[String],    // For staggered and hexagonal maps, determines which axis ("x" or "y") is staggered
                           staggerindex: Option[String],   // For staggered and hexagonal maps, determines whether the "even" or "odd" indexes along the staggered axis are shifted.
                           backgroundcolor: Option[String] // #AARRGGBB
                         ) derives CanEqual {
  def toGrid[A](mapper: Int => A): Option[TiledGridMap[A]] = {

    def toGridLayer(tiledLayer: TiledLayer): TiledGridLayer[A] =
      TiledGridLayer(
        rec(tiledLayer.data.map(mapper).zipWithIndex, tiledLayer.width, Nil),
        tiledLayer.width,
        tiledLayer.height
      )

    given CanEqual[List[(A, Int)], List[(A, Int)]] = CanEqual.derived

    @tailrec
    def rec(remaining: List[(A, Int)], columnCount: Int, acc: List[TiledGridCell[A]]): List[TiledGridCell[A]] =
      remaining match {
        case Nil =>
          acc

        case (a, i) :: xs =>
          rec(xs, columnCount, acc ++ List(TiledGridCell(i % columnCount, i / columnCount, a)))
      }

    layers match {
      case Nil =>
        None

      case l :: ls =>
        Option(
          TiledGridMap[A](
            NonEmptyList(toGridLayer(l), ls.map(toGridLayer))
          )
        )

    }
  }

  def parseAnimations(): Option[Seq[Iterable[Animation]]] =
    TiledMap.parseAnimations(this)

  def parseObjects(): List[TiledMapObject] =
    TiledMap.parseObjects(this) 

  def toGroup(assetName: AssetName): Option[Group] =
    TiledMap.toGroup(this, assetName)

  def toCLoneGroup(assetName: AssetName): Option[(List[CloneBlank], Group)] =
    TiledMap.toCloneGroup(this, assetName)
}

final case class TiledLayer(
                             name: String,
                             data: List[Int],
                             x: Int,
                             y: Int,
                             width: Int,
                             height: Int,
                             opacity: Double,
                             `type`: String, // tilelayer, objectgroup, or imagelayer
                             visible: Boolean,
                             objects: Option[List[TiledMapObject]],
                           ) derives CanEqual

object TiledLayer {
  def apply(name: String,
             data: List[Int],
             x: Int,
             y: Int,
             width:Int,
             height: Int,
             opacity: Double,
             `type`: String, // tilelayer, objectgroup, or imagelayer
             visible: Boolean): TiledLayer =
    new TiledLayer(name, data, x, y, width, height, opacity, `type`, visible, Some(List[TiledMapObject]()))
}

final case class TiledMapObject(
  height: Double,
  id: Int,
  rotation: Double,
  `type`: Option[String],
  visible: Boolean,
  width: Double,
  x: Double,
  y: Double
)

final case class TileSet(
                          columns: Option[Int],
                          firstgid: Int,
                          image: Option[String],
                          imageheight: Option[Int],
                          imagewidth: Option[Int],
                          margin: Option[Int],
                          name: Option[String],
                          spacing: Option[Int],
                          terrains: Option[List[TiledTerrain]],
                          tilecount: Option[Int],
                          tileheight: Option[Int],
                          tiles: Option[List[TiledTerrainCorner]],
                          tilewidth: Option[Int],
                          source: Option[String]
                        ) derives CanEqual

final case class TiledFrame(duration: Int, tileid: Int) derives CanEqual

final case class TiledTerrainCorner(id: Int, animation: Option[List[TiledFrame]]) derives CanEqual

final case class TiledTerrain(name: String, tile: Int) derives CanEqual 

object TiledMap {

  private def fromIndex(index: Int, gridWidth: Int): Point =
    Point(
      x = index % gridWidth,
      y = index / gridWidth
    )

  private def parseObjects(tiledMap: TiledMap): List[TiledMapObject] =
    tiledMap.layers.filter(_.`type` == "objectgroup").flatMap { layer =>
      layer.objects.getOrElse(List[TiledMapObject]())
    }

  private def parseAnimations(tiledMap: TiledMap): Option[Seq[Iterable[Animation]]] =
    tiledMap.tilesets.headOption.flatMap(_.columns).map { tileSheetColumnCount =>
      val tileSize: Size = Size(tiledMap.tilewidth, tiledMap.tileheight)
      tiledMap.tilesets.flatMap {
        tileset =>
          tileset.tiles.map(tile => {
            tile.flatMap {
              tl => tl.animation.map { a =>
                val framesSeq: Seq[Frame] = a.map { f =>
                  Frame(Rectangle(fromIndex(f.tileid, tileSheetColumnCount) * tileSize.toPoint, tileSize), Millis(f.duration.toLong))
                }
                Animation(
                  AnimationKey(tl.id.toString),
                  frameOne = framesSeq.headOption.getOrElse(Frame(Rectangle(fromIndex(0, tileSheetColumnCount) * tileSize.toPoint, tileSize), Millis(0))),
                  frames = framesSeq.drop(1): _*
                )
              }
            }
          })

      }
    }

  private def toCloneGroup(tiledMap: TiledMap, assetName: AssetName): Option[(List[CloneBlank], Group)] = 
    tiledMap.tilesets.headOption.flatMap(_.columns).map { tileSheetColumnCount =>

        val tileSize: Size = Size(tiledMap.tilewidth, tiledMap.tileheight)

        val firstgid: Int =  tiledMap.tilesets.map({tileset => tileset.firstgid}).head //TODO: WHAT IF MULTIPLE TILESETS? OR default to 1 ??

        val animations: Map[Int, Option[List[TiledFrame]]] = tiledMap.tilesets.flatMap({ tileset =>
            tileset.tiles.flatMap(tile => {
                Option(tile.map {
                        case tiledTerrainCorner: TiledTerrainCorner if tiledTerrainCorner.animation.nonEmpty =>
                            (tiledTerrainCorner.id, tiledTerrainCorner.animation)
                        case tiledTerrainCorner: TiledTerrainCorner => (tiledTerrainCorner.id, None)
                    })
                })
            }).flatten.toMap

        val animationSprites: Map[String, Sprite[Material.Bitmap]] = 
            (for (a <- animations) yield {
                val keyString: String = a.toString
                val key = AnimationKey(keyString)
                val sprite: Sprite[Material.Bitmap] = Sprite(BindingKey(keyString), 0, 0, 1, key, Material.Bitmap(assetName)).play()
                (keyString -> sprite)
            }).toMap

        val animationCloneBlanks: List[CloneBlank] = (for ((k, s) <- animationSprites) yield { CloneBlank(CloneId(k), s) }).toList

        val tileMapGraphic: Graphic[Material.Bitmap] = Graphic(tiledMap.width * tiledMap.tilewidth, tiledMap.height * tiledMap.tileheight, Material.Bitmap(assetName))

        val tileMapCloneBlanks: List[CloneBlank] = List(CloneBlank(CloneId("graphic"), tileMapGraphic))

        val layers = tiledMap.layers.filter(_.`type` == "tilelayer").map { layer =>
            val tilesInUse: Map[Int, String] =
                layer.data.toSet.foldLeft(Map.empty[Int, String]) { (tiles, i) =>
                    tiles ++ Map(
                        i ->
                            {
                            if(animations.contains(i - firstgid)) {
                                if(animations(i - firstgid).nonEmpty) {
                                    (i - firstgid).toString
                                //Sprite(BindingKey((i - firstgid).toString + System.currentTimeMillis().hashCode().toString), 0, 0, 1, key, Material.Bitmap(assetName))
                            } else {
                                /*Graphic(Rectangle(Point.zero, tileSize), 1, Material.Bitmap(assetName))
                                  .withCrop(
                                    Rectangle(fromIndex(i - firstgid, tileSheetColumnCount) * tileSize.toPoint, tileSize)
                                  )*/
                                    "graphic"
                                }
                            } else {
                                /*Graphic(Rectangle(Point.zero, tileSize), 1, Material.Bitmap(assetName))
                                .withCrop(
                                  Rectangle(fromIndex(i - firstgid, tileSheetColumnCount) * tileSize.toPoint, tileSize)
                                )*/
                                    "graphic"
                            }
                        }
                    )
                }

            val cloneBatches: List[SceneNode] = (
                for {
                    layerDataChunk <- layer.data.zipWithIndex.grouped(512)
                    (key, sprite) <- animationSprites
                } 
                yield {
                    CloneBatch(CloneId(key),
                    layerDataChunk.flatMap {
                        case (tileIndex, positionIndex) =>
                            if (tileIndex == 0) Nil
                            else
                                tilesInUse
                                .get(tileIndex)
                                .map {
                                    case c:String if c == key => {
                                        val p = fromIndex(positionIndex, tiledMap.width) * tileSize.toPoint
                                        List(CloneBatchData(p.x, p.y))
                                    }
                                    case _ => Nil
                                }
                                .getOrElse(Nil)
                            }.toArray)
                }).toList

            val cloneTiles: List[SceneNode] = (
                for {
                    layerDataChunk <- layer.data.zipWithIndex.grouped(512)
                } yield {
                    CloneTiles(CloneId("graphic"),
                    layerDataChunk.flatMap {
                        case (tileIndex, positionIndex) =>
                            if (tileIndex == 0) Nil
                            else
                                tilesInUse
                                .get(tileIndex)
                                .map {
                                    case c:String if c == "graphic" => {
                                        val tlSize = tileSize.toPoint
                                        val pos = fromIndex(positionIndex, tiledMap.width) * tlSize
                                        val cropPos = fromIndex(tileIndex - firstgid, tileSheetColumnCount) * tlSize
                                        List(CloneTileData(pos.x, pos.y, Radians.zero, 1, 1, cropPos.x, cropPos.y, tlSize.x, tlSize.y))                     
                                    }                         
                                    case _ => Nil
                                }.getOrElse(Nil)}.toArray)
                }).toList

            val clones = cloneBatches ++ cloneTiles
            Group(clones)
            }

        (tileMapCloneBlanks ++ animationCloneBlanks, Group(layers))
        }

  private def toGroup(tiledMap: TiledMap, assetName: AssetName): Option[Group] =
    tiledMap.tilesets.headOption.flatMap(_.columns).map { tileSheetColumnCount =>
        val tileSize: Size = Size(tiledMap.tilewidth, tiledMap.tileheight)
        val firstgid: Int =  tiledMap.tilesets.map({tileset => tileset.firstgid}).head //TODO: WHAT IF MULTIPLE TILESETS? OR default to 1 ??
        val animations: Map[Int, Option[List[TiledFrame]]] = tiledMap.tilesets.flatMap({
        tileset =>
            tileset.tiles.flatMap(tile => {
                Option(tile.map {
                case tiledTerrainCorner: TiledTerrainCorner if tiledTerrainCorner.animation.nonEmpty =>
                    (tiledTerrainCorner.id, tiledTerrainCorner.animation)
                case tiledTerrainCorner: TiledTerrainCorner => (tiledTerrainCorner.id, None)
                })
            })
        }).flatten.toMap

        val layers = tiledMap.layers.filter(_.`type` == "tilelayer").map { layer =>

            val tilesInUse: Map[Int, SceneNode] =
                layer.data.toSet.foldLeft(Map.empty[Int, SceneNode]) { (tiles, i) =>
                    tiles ++ Map(
                    i ->
                        {
                        if(animations.contains(i - firstgid)) {
                          if(animations(i - firstgid).nonEmpty) {
                            val key = AnimationKey((i - firstgid).toString)
                            Sprite(BindingKey((i - firstgid).toString + System.currentTimeMillis().hashCode().toString), 0, 0, 1, key, Material.Bitmap(assetName))
                          } else {
                            Graphic(Rectangle(Point.zero, tileSize), 1, Material.Bitmap(assetName))
                              .withCrop(
                                Rectangle(fromIndex(i - firstgid, tileSheetColumnCount) * tileSize.toPoint, tileSize)
                              )
                          }
                        } else {
                          Graphic(Rectangle(Point.zero, tileSize), 1, Material.Bitmap(assetName))
                            .withCrop(
                              Rectangle(fromIndex(i - firstgid, tileSheetColumnCount) * tileSize.toPoint, tileSize)
                            )
                        }
                    })
                }

            Group(
                layer.data.zipWithIndex.flatMap {
                    case (tileIndex, positionIndex) =>
                    if (tileIndex == 0) Nil
                    else
                        tilesInUse
                        .get(tileIndex)
                        .map {
                            case g:Sprite[_] => List(g.moveTo(fromIndex(positionIndex, tiledMap.width) * tileSize.toPoint).play())
                            case g:Graphic[_] => List(g.moveTo(fromIndex(positionIndex, tiledMap.width) * tileSize.toPoint))
                            case g:SceneNode => List(g)
                        }
                        .getOrElse(Nil)
                    }
                )
            }
            
        Group(layers)
    }
}

final case class TiledGridMap[A](layers: NonEmptyList[TiledGridLayer[A]]) derives CanEqual {

  lazy val toListPerLayer: NonEmptyList[List[TiledGridCell[A]]] =
    layers.map(_.grid)

  lazy val toList2DPerLayer: NonEmptyList[List[List[TiledGridCell[A]]]] = {
    given CanEqual[List[TiledGridCell[A]], List[TiledGridCell[A]]] = CanEqual.derived

    @tailrec
    def rec(
        remaining: List[TiledGridCell[A]],
        columnCount: Int,
        current: List[TiledGridCell[A]],
        acc: List[List[TiledGridCell[A]]]
    ): List[List[TiledGridCell[A]]] =
      remaining match {
        case Nil =>
          acc.reverse

        case x :: xs if x.column == columnCount - 1 =>
          rec(xs, columnCount, Nil, (current ++ List(x)) :: acc)

        case x :: xs =>
          rec(xs, columnCount, current ++ List(x), acc)
      }

    layers.map { layer =>
      rec(layer.grid, layer.columnCount, Nil, Nil)
    }
  }

}
final case class TiledGridLayer[A](grid: List[TiledGridCell[A]], columnCount: Int, rowCount: Int) derives CanEqual
final case class TiledGridCell[A](column: Int, row: Int, tile: A) derives CanEqual {
  lazy val x: Int = column
  lazy val y: Int = row
}

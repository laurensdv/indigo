package indigo.shared.formats

import indigo.shared.EqualTo._
import indigo.shared.animation.{Animation, Frame, AnimationKey}
import indigo.shared.assets.AssetName
import indigo.shared.time.Millis
import indigo.shared.collections.NonEmptyList
import indigo.shared.datatypes.{BindingKey, Material, Point, Rectangle}
import indigo.shared.scenegraph.{Graphic, Group, Renderable, Sprite}
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
                         ) {

  def toGrid[A](mapper: Int => A): Option[TiledGridMap[A]] = {

    def toGridLayer(tiledLayer: TiledLayer): TiledGridLayer[A] =
      TiledGridLayer(
        rec(tiledLayer.data.map(mapper).zipWithIndex, tiledLayer.width, Nil),
        tiledLayer.width,
        tiledLayer.height
      )

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

  def parseAnimations(assetName: AssetName): Option[Seq[Iterable[Animation]]] =
    TiledMap.parseAnimations(this, assetName)

  def parseObjects(): List[TiledMapObject] =
    TiledMap.parseObjects(this) 

  def toGroup(assetName: AssetName): Option[Group] =
    TiledMap.toGroup(this, assetName)
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
                           )

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
                        )

final case class TiledTerrain(name: String, tile: Int)
final case class TiledFrame(duration: Int, tileid: Int)
final case class TiledTerrainCorner(id: Int, animation: Option[List[TiledFrame]])

object TiledMap {

  private def fromIndex(index: Int, gridWidth: Int): Point =
    Point(
      x = index % gridWidth,
      y = index / gridWidth
    )

    def parseObjects(tiledMap: TiledMap): List[TiledMapObject] =
    tiledMap.layers.filter(_.`type`==="objectgroup").flatMap { layer =>
      layer.objects.getOrElse(List[TiledMapObject]())
    }

    def parseAnimations(tiledMap: TiledMap, assetName: AssetName): Option[Seq[Iterable[Animation]]] =
    tiledMap.tilesets.headOption.flatMap(_.columns).map { tileSheetColumnCount =>
      val tileSize: Point = Point(tiledMap.tilewidth, tiledMap.tileheight)
      tiledMap.tilesets.flatMap {
        tileset =>
          tileset.tiles.map(tile => {
            tile.flatMap {
              tl => tl.animation.map { a =>
                val framesSeq: Seq[Frame] = a.map { f =>
                  Frame(Rectangle(fromIndex(f.tileid, tileSheetColumnCount) * tileSize, tileSize), Millis(f.duration.toLong))
                }
                Animation(
                  AnimationKey(tl.id.toString),
                  Material.Textured(assetName),
                  frameOne = framesSeq.headOption.getOrElse(Frame(Rectangle(fromIndex(0, tileSheetColumnCount) * tileSize, tileSize), Millis(0))),
                  frames = framesSeq.drop(1): _*
                )
              }
            }
          })
      }
    }

    def toGroup(tiledMap: TiledMap, assetName: AssetName): Option[Group] =
      tiledMap.tilesets.headOption.flatMap(_.columns).map { tileSheetColumnCount =>
        val tileSize: Point = Point(tiledMap.tilewidth, tiledMap.tileheight)
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

        val layers = tiledMap.layers.filter(_.`type`==="tilelayer").map { layer =>
          val tilesInUse: Map[Int, Renderable] =
            layer.data.toSet.foldLeft(Map.empty[Int, Renderable]) { (tiles, i) =>
              tiles ++ Map(
                i ->
                  {
                    if(animations.contains(i - 1)) {
                      if(animations(i - 1).nonEmpty) {
                        val key = AnimationKey((i - 1).toString)
                        Sprite(BindingKey((i - 1).toString + System.currentTimeMillis().hashCode().toString), 0, 0, 1, key)
                      } else {
                        Graphic(Rectangle(Point.zero, tileSize), 1, Material.Textured(assetName))
                          .withCrop(
                            Rectangle(fromIndex(i - 1, tileSheetColumnCount) * tileSize, tileSize)
                          )
                      }
                    } else {
                      Graphic(Rectangle(Point.zero, tileSize), 1, Material.Textured(assetName))
                        .withCrop(
                          Rectangle(fromIndex(i - 1, tileSheetColumnCount) * tileSize, tileSize)
                        )
                    }
                  }

              )
            }

          Group(
            layer.data.zipWithIndex.flatMap {
              case (tileIndex, positionIndex) =>
                if (tileIndex === 0) Nil
                else
                  tilesInUse
                    .get(tileIndex)
                    .map(g => g.moveTo(fromIndex(positionIndex, tiledMap.width) * tileSize))
                    .map {
                      case g:Sprite => List(g.play())
                      case g:Renderable => List(g)
                    }
                    .getOrElse(Nil)
            }
          )
        }

        Group(layers)
      }

}

final case class TiledGridMap[A](layers: NonEmptyList[TiledGridLayer[A]]) {

  lazy val toListPerLayer: NonEmptyList[List[TiledGridCell[A]]] =
    layers.map(_.grid)

  lazy val toList2DPerLayer: NonEmptyList[List[List[TiledGridCell[A]]]] = {
    @tailrec
    def rec(remaining: List[TiledGridCell[A]], columnCount: Int, current: List[TiledGridCell[A]], acc: List[List[TiledGridCell[A]]]): List[List[TiledGridCell[A]]] =
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
final case class TiledGridLayer[A](grid: List[TiledGridCell[A]], columnCount: Int, rowCount: Int)
final case class TiledGridCell[A](column: Int, row: Int, tile: A) {
  lazy val x: Int = column
  lazy val y: Int = row
}

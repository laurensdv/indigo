package indigo.shared.formats

import indigo.shared.animation.Animation
import indigo.shared.animation.AnimationKey
import indigo.shared.animation.Frame
import indigo.shared.assets.AssetName
import indigo.shared.collections.Batch
import indigo.shared.collections.NonEmptyBatch
import indigo.shared.datatypes.BindingKey
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Rectangle
import indigo.shared.datatypes.Size
import indigo.shared.materials.Material
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
    nextobjectid: Int, // Stores the next available ID for new objects. This number is stored to prevent reuse of the same ID after objects have been removed.
    orientation: String, // orthogonal, isometric, staggered and hexagonal
    renderorder: String, // right-down (the default), right-up, left-down and left-up. In all cases, the map is drawn row-by-row.
    tiledversion: String,
    tilewidth: Int,
    tileheight: Int,
    tilesets: List[TileSet],
    `type`: String, // "map"
    hexsidelength: Option[Int],
    staggeraxis: Option[String], // For staggered and hexagonal maps, determines which axis ("x" or "y") is staggered
    staggerindex: Option[
      String
    ], // For staggered and hexagonal maps, determines whether the "even" or "odd" indexes along the staggered axis are shifted.
    backgroundcolor: Option[String] // #AARRGGBB
) derives CanEqual {

  private[formats] lazy val tileAnimations: Map[Int, Option[List[TiledFrame]]] =
    tilesets
      .flatMap { tileset =>
        tileset.tiles.flatMap { tile =>
          Option(tile.map {
            case ttc if ttc.animation.nonEmpty => (ttc.id, ttc.animation)
            case ttc                           => (ttc.id, None)
          })
        }
      }
      .flatten
      .toMap
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
            NonEmptyBatch(toGridLayer(l), Batch.fromList(ls.map(toGridLayer)))
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
    objects: Option[List[TiledMapObject]]
) derives CanEqual

object TiledLayer {
  def apply(
      name: String,
      data: List[Int],
      x: Int,
      y: Int,
      width: Int,
      height: Int,
      opacity: Double,
      `type`: String, // tilelayer, objectgroup, or imagelayer
      visible: Boolean
  ): TiledLayer =
    new TiledLayer(name, data, x, y, width, height, opacity, `type`, visible, Some(List[TiledMapObject]()))
}

final case class TiledMapObjectPolygonPoint(x: Double, y: Double)

final case class TiledMapObject(
    id: Int,
    rotation: Double,
    `type`: Option[String],
    visible: Boolean,
    height: Double,
    width: Double,
    x: Double,
    y: Double,
    polygon: Option[List[TiledMapObjectPolygonPoint]]
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
      tiledMap.tilesets.flatMap { tileset =>
        tileset.tiles.map(tile =>
          tile.flatMap { tl =>
            tl.animation.map { a =>
              val framesSeq: Seq[Frame] = a.map { f =>
                Frame(
                  Rectangle(fromIndex(f.tileid, tileSheetColumnCount) * tileSize.toPoint, tileSize),
                  Millis(f.duration.toLong)
                )
              }
              Animation(
                AnimationKey(tl.id.toString),
                frameOne = framesSeq.headOption.getOrElse(
                  Frame(Rectangle(fromIndex(0, tileSheetColumnCount) * tileSize.toPoint, tileSize), Millis(0))
                ),
                frames = framesSeq.drop(1)*
              )
            }
          }
        )

      }
    }

  private def toGroup(tiledMap: TiledMap, assetName: AssetName): Option[Group] =
    tiledMap.tilesets.headOption.flatMap(_.columns).map { tileSheetColumnCount =>
      val tileSize: Size = Size(tiledMap.tilewidth, tiledMap.tileheight)
      val firstgid: Int  = tiledMap.tilesets.map(_.firstgid).head
      val animations     = tiledMap.tileAnimations

      def buildNode(i: Int): SceneNode =
        val localId = i - firstgid
        if animations.contains(localId) && animations(localId).nonEmpty then
          Sprite(
            BindingKey(s"${assetName.toString}_$localId"),
            0,
            0,
            AnimationKey(localId.toString),
            Material.Bitmap(assetName)
          )
        else
          Graphic(Rectangle(Point.zero, tileSize), Material.Bitmap(assetName))
            .withCrop(Rectangle(fromIndex(localId, tileSheetColumnCount) * tileSize.toPoint, tileSize))

      val layers: Batch[Group] = Batch.fromList(tiledMap.layers.filter(_.`type` == "tilelayer")).map { layer =>

        val tilesInUse: Map[Int, SceneNode] =
          Map.from(
            layer.data.iterator.filter(_ != 0).toSet.iterator.map(i => i -> buildNode(i))
          )

        Group(
          Batch.fromIterator(
            layer.data.iterator.zipWithIndex.flatMap { case (tileIndex, positionIndex) =>
              if tileIndex == 0 then Iterator.empty
              else
                tilesInUse.get(tileIndex) match
                  case Some(g: Sprite[_]) =>
                    Iterator.single(g.moveTo(fromIndex(positionIndex, tiledMap.width) * tileSize.toPoint).play())
                  case Some(g: Graphic[_]) =>
                    Iterator.single(g.moveTo(fromIndex(positionIndex, tiledMap.width) * tileSize.toPoint))
                  case Some(g: SceneNode) => Iterator.single(g)
                  case None               => Iterator.empty
            }
          )
        )
      }

      Group(layers)
    }
}

final case class TiledGridMap[A](layers: NonEmptyBatch[TiledGridLayer[A]]) derives CanEqual {

  lazy val toListPerLayer: NonEmptyBatch[List[TiledGridCell[A]]] =
    layers.map(_.grid)

  lazy val toList2DPerLayer: NonEmptyBatch[List[List[TiledGridCell[A]]]] = {
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

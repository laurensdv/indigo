package indigo.shared.formats

import indigo.shared.assets.AssetName
import indigo.shared.collections.Batch
import indigo.shared.collections.NonEmptyBatch
import indigo.shared.datatypes.BindingKey
import indigo.shared.datatypes.Point
import indigo.shared.scenegraph.Graphic
import indigo.shared.scenegraph.Group
import indigo.shared.scenegraph.Sprite

@SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
class TiledMapTests extends munit.FunSuite {

  test("should be able to convert to a TiledGridMap.identity (Int)") {
    val actual =
      TiledSamples.tiledMap
        .toGrid[Int](identity[Int])
        .get
        .toListPerLayer
        .head
        .map(_.tile)

    val expected: List[Int] =
      TiledSamples.gridMapInt.layers.head.grid.map(_.tile)

    assertEquals(actual, expected)

  }

  test("should be able to convert to a TiledGridMap.with mapping") {
    // Using nowarn as it's a partial match and I want it
    // to blow up if it finds anything else.
    val matcher: ((Int, TileTypes)) => Boolean = {
      case (0, TileTypes.Empty)           => true
      case (i, TileTypes.Solid) if i != 0 => true
      case (i, TileTypes.Empty) if i != 0 => false
      case (i, TileTypes.Solid) if i == 0 => false
      case _ =>
        throw new Exception("test failed")
    }

    val actual =
      TiledSamples.tiledMap
        .toGrid[TileTypes](TiledSamples.mapping)
        .get
        .toListPerLayer
        .head
        .map(_.tile)

    assertEquals(
      TiledSamples.gridMapInt.layers.head.grid
        .map(_.tile)
        .zip(actual)
        .forall(matcher),
      true
    )

  }

  test("should be able to convert to a TiledGridMap.to 2D grid (int)") {
    val actual: List[List[Int]] =
      TiledSamples.tiledMap
        .toGrid[Int](identity[Int])
        .get
        .toList2DPerLayer
        .head
        .map(_.map(_.tile))

    val expected: List[List[Int]] =
      TiledSamples.gridMapInt2D

    assertEquals(actual, expected)
  }

  test("should be able to convert to a Group of graphics") {
    val actual: Group =
      TiledSamples.tiledMap.toGroup(AssetName("test")).get

    actual.children.head match {
      case g: Group =>
        // Only 3 tiles have contents.
        val graphics: Batch[Graphic[?]] =
          g.children.collect { case graphic: Graphic[_] => graphic }

        assertEquals(graphics.length, 3)
        assertEquals(graphics(0).position, Point(32, 64))
        assertEquals(graphics(1).position, Point(64, 64))
        assertEquals(graphics(2).position, Point(64, 96))

      case _ =>
        throw new Exception("failed")
    }
  }

  test("animated tile BindingKey must be stable across multiple toGroup calls") {
    val assetName = AssetName("tileset.png")
    val group1    = TiledSamples.animatedTiledMap.toGroup(assetName).get
    val group2    = TiledSamples.animatedTiledMap.toGroup(assetName).get

    def collectKeys(g: Group): Batch[BindingKey] =
      g.children.head match {
        case layer: Group => layer.children.collect { case s: Sprite[_] => s.bindingKey }
        case _            => throw new Exception("expected layer Group")
      }

    val keys1 = collectKeys(group1)
    val keys2 = collectKeys(group2)

    assert(keys1.nonEmpty, "expected at least one animated sprite")
    assertEquals(keys1, keys2)
  }

  test("animated tile BindingKey must differ when assetName differs") {
    val group1 = TiledSamples.animatedTiledMap.toGroup(AssetName("map-a.png")).get
    val group2 = TiledSamples.animatedTiledMap.toGroup(AssetName("map-b.png")).get

    def firstSpriteKey(g: Group): BindingKey =
      g.children.head match {
        case layer: Group =>
          layer.children.collectFirst { case s: Sprite[_] => s.bindingKey }
            .getOrElse(throw new Exception("no sprite found"))
        case _ => throw new Exception("expected layer Group")
      }

    assertNotEquals(firstSpriteKey(group1), firstSpriteKey(group2))
  }

  test("tile with animation entry set to None must produce a Graphic, not a Sprite") {
    val group = TiledSamples.animatedTiledMap.toGroup(AssetName("tileset.png")).get

    group.children.head match {
      case layer: Group =>
        val sprites  = layer.children.collect { case s: Sprite[_]  => s }
        val graphics = layer.children.collect { case g: Graphic[_] => g }
        // tile localId=0 is animated → Sprite; tile localId=1 has animation=None → Graphic
        assert(sprites.nonEmpty,  "expected a Sprite for the animated tile")
        assert(graphics.nonEmpty, "expected a Graphic for the tile with animation=None")
      case _ =>
        throw new Exception("expected layer Group")
    }
  }

}

sealed trait TileTypes derives CanEqual
object TileTypes {
  case object Empty extends TileTypes
  case object Solid extends TileTypes
}

object TiledSamples {

  val mapping: Int => TileTypes = {
    case 0 => TileTypes.Empty
    case _ => TileTypes.Solid
  }

  val gridMapInt: TiledGridMap[Int] =
    TiledGridMap(
      NonEmptyBatch(
        TiledGridLayer(
          List(
            TiledGridCell(0, 0, 0),
            TiledGridCell(1, 0, 0),
            TiledGridCell(2, 0, 0),
            TiledGridCell(3, 0, 0),
            TiledGridCell(0, 1, 0),
            TiledGridCell(1, 1, 0),
            TiledGridCell(2, 1, 0),
            TiledGridCell(3, 1, 0),
            TiledGridCell(0, 2, 0),
            TiledGridCell(1, 2, 2),
            TiledGridCell(2, 2, 1),
            TiledGridCell(3, 2, 0),
            TiledGridCell(0, 3, 0),
            TiledGridCell(1, 3, 0),
            TiledGridCell(2, 3, 1),
            TiledGridCell(3, 3, 0)
          ),
          4,
          4
        )
      )
    )

  val gridMapInt2D: List[List[Int]] =
    List(
      List(
        TiledGridCell(0, 0, 0),
        TiledGridCell(1, 0, 0),
        TiledGridCell(2, 0, 0),
        TiledGridCell(3, 0, 0)
      ).map(_.tile),
      List(
        TiledGridCell(0, 1, 0),
        TiledGridCell(1, 1, 0),
        TiledGridCell(2, 1, 0),
        TiledGridCell(3, 1, 0)
      ).map(_.tile),
      List(
        TiledGridCell(0, 2, 0),
        TiledGridCell(1, 2, 2),
        TiledGridCell(2, 2, 1),
        TiledGridCell(3, 2, 0)
      ).map(_.tile),
      List(
        TiledGridCell(0, 3, 0),
        TiledGridCell(1, 3, 0),
        TiledGridCell(2, 3, 1),
        TiledGridCell(3, 3, 0)
      ).map(_.tile)
    )

  // tile localId=0 is animated (2-frame); tile localId=1 has an entry with animation=None
  val animatedTiledMap: TiledMap =
    TiledMap(
      4, 4, false,
      List(
        TiledLayer(
          "Tile Layer 1",
          List(
            1, 2, 0, 0,
            0, 0, 0, 0,
            0, 0, 0, 0,
            0, 0, 0, 0
          ),
          0, 0, 4, 4, 1, "tilelayer", true
        )
      ),
      1, "orthogonal", "right-down", "1.3.2",
      32, 32,
      List(
        TileSet(
          Some(4),
          1,
          Some("tileset.png"),
          Some(128),
          Some(128),
          Some(0),
          Some("Test Tileset"),
          Some(0),
          None,
          Some(16),
          Some(32),
          Some(List(
            TiledTerrainCorner(0, Some(List(TiledFrame(100, 0), TiledFrame(100, 1)))),
            TiledTerrainCorner(1, None)
          )),
          Some(32),
          None
        )
      ),
      "map",
      None, None, None, None
    )

  val tiledMap: TiledMap =
    TiledMap(
      4,
      4,
      false,
      List(
        TiledLayer(
          "Tile Layer 1",
          gridMapInt.layers.head.grid.map(_.tile),
          0,
          0,
          4,
          4,
          1,
          "tilelayer",
          true
        )
      ),
      1,
      "orthogonal",
      "right-down",
      "1.3.2",
      32,
      32,
      List(
        TileSet(
          Some(17),
          1,
          Some("terrain.png"),
          Some(160),
          Some(544),
          Some(0),
          Some("Palm Island"),
          Some(0),
          None,
          Some(85),
          Some(32),
          None,
          Some(32),
          None
        )
      ),
      "map",
      None,
      None,
      None,
      None
    )

}

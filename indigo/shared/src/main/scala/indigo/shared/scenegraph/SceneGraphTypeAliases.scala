package indigo.shared.scenegraph

import indigo.shared.scenegraph

trait SceneGraphTypeAliases {

  type SceneUpdateFragment = scenegraph.SceneUpdateFragment
  val SceneUpdateFragment: scenegraph.SceneUpdateFragment.type = scenegraph.SceneUpdateFragment

  type ScreenEffects = scenegraph.ScreenEffects
  val ScreenEffects: scenegraph.ScreenEffects.type = scenegraph.ScreenEffects

  type SceneLayer = scenegraph.SceneLayer
  val SceneLayer: scenegraph.SceneLayer.type = scenegraph.SceneLayer

  type SceneGraphNode = scenegraph.SceneGraphNode
  val SceneGraphNode: scenegraph.SceneGraphNode.type = scenegraph.SceneGraphNode

  type Renderable = scenegraph.Renderable

  // Audio
  type SceneAudio = scenegraph.SceneAudio
  val SceneAudio: scenegraph.SceneAudio.type = scenegraph.SceneAudio

  type Volume = indigo.shared.audio.Volume
  val Volume: indigo.shared.audio.Volume.type = indigo.shared.audio.Volume

  type Track = indigo.shared.audio.Track
  val Track: indigo.shared.audio.Track.type = indigo.shared.audio.Track

  type PlaybackPattern = scenegraph.PlaybackPattern
  val PlaybackPattern: scenegraph.PlaybackPattern.type = scenegraph.PlaybackPattern

  type SceneAudioSource = scenegraph.SceneAudioSource
  val SceneAudioSource: scenegraph.SceneAudioSource.type = scenegraph.SceneAudioSource

  // Animation
  type Animation = indigo.shared.animation.Animation
  val Animation: indigo.shared.animation.Animation.type = indigo.shared.animation.Animation

  type Cycle = indigo.shared.animation.Cycle
  val Cycle: indigo.shared.animation.Cycle.type = indigo.shared.animation.Cycle

  type CycleLabel = indigo.shared.animation.CycleLabel
  val CycleLabel: indigo.shared.animation.CycleLabel.type = indigo.shared.animation.CycleLabel

  type Frame = indigo.shared.animation.Frame
  val Frame: indigo.shared.animation.Frame.type = indigo.shared.animation.Frame

  type AnimationKey = indigo.shared.animation.AnimationKey
  val AnimationKey: indigo.shared.animation.AnimationKey.type = indigo.shared.animation.AnimationKey

  type AnimationAction = indigo.shared.animation.AnimationAction
  val AnimationAction: indigo.shared.animation.AnimationAction.type = indigo.shared.animation.AnimationAction

  // Primitives
  type Sprite = scenegraph.Sprite
  val Sprite: scenegraph.Sprite.type = scenegraph.Sprite

  type Text = scenegraph.Text
  val Text: scenegraph.Text.type = scenegraph.Text

  type Graphic = scenegraph.Graphic
  val Graphic: scenegraph.Graphic.type = scenegraph.Graphic

  type Group = scenegraph.Group
  val Group: scenegraph.Group.type = scenegraph.Group

  // Clones
  type CloneBlank = scenegraph.CloneBlank
  val CloneBlank: scenegraph.CloneBlank.type = scenegraph.CloneBlank

  type CloneId = scenegraph.CloneId
  val CloneId: scenegraph.CloneId.type = scenegraph.CloneId

  type Clone = scenegraph.Clone
  val Clone: scenegraph.Clone.type = scenegraph.Clone

  type CloneBatch = scenegraph.CloneBatch
  val CloneBatch: scenegraph.CloneBatch.type = scenegraph.CloneBatch

  type CloneTransformData = scenegraph.CloneTransformData
  val CloneTransformData: scenegraph.CloneTransformData.type = scenegraph.CloneTransformData

  // Lights
  type PointLight = scenegraph.PointLight
  val PointLight: scenegraph.PointLight.type = scenegraph.PointLight

  type SpotLight = scenegraph.SpotLight
  val SpotLight: scenegraph.SpotLight.type = scenegraph.SpotLight

  type DirectionLight = scenegraph.DirectionLight
  val DirectionLight: scenegraph.DirectionLight.type = scenegraph.DirectionLight

}

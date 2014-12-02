package filesystem

import scala.collection.mutable.Set
/**
 * Created by dimitar on 12/1/14.
 */

class INode(name: String, directory: Boolean) {
  var content: String = ""
  var children = Set.empty[INode]

  val fileName = name
  val isDir = directory
  def addChild(child: INode) = children.add(child)
}

package filesystem

import scala.collection.mutable.Set
import scala.collection.mutable.Map

/**
 * Created by dimitar on 12/1/14.
 * A basic file system that supports the commands: ls, pwd, mkdir, create, rm, cd and cat
 */

object FileSystem {

  var currentDir = "/"
  val index = Map.empty[String, INode]

  def main(args: Array[String]) {

    val top = new INode("/", true)
    index.put("/", top)

    var loop = true
    while (loop) {
      val command = readLine().split(" ")
      command match {
        case Array("ls") => ls()
        case Array("rm", file: String) => rm(file)
        case Array("pwd") => println(currentDir)
        case Array("cd") => cd("/")
        case Array("create", file: String, c: String) => create(file, c)
        case Array("create", file: String) => create(file, "")
        case Array("cd", path: String) => cd(path)
        case Array("mkdir", dir: String) => mkdir(dir)
        case Array("cat", file: String) => cat(file)
        case Array("quit") => loop = false
        case _ => println("Command not found.")
      }
    }
  }

  private def ls(): Unit = {
    val currentDirName = getCurrentDirName()
    currentDirName.children.foreach(f => println(f.fileName))
  }

  private def cd(dest: String): Unit = {
    if (dest.equals("/")) {
      currentDir = "/"
      return
    }
    val target = dest.trim.split("/").filter(e => !e.trim.isEmpty)
    val start = {
      dest match {
        case d: String if d.startsWith("/") => "/"
        case d: String if currentDir.length > 1 => currentDir.split("/").filter(e => !e.trim.isEmpty).last
        case _ => currentDir
      }
    }

    val deepest = getDeepestDir(target , index.get(start).get.children)

    if (deepest.isEmpty) {
      println("Directory " + dest + " does not exist.")
      return
    }

    currentDir = {
      if (dest.startsWith("/")) {
        dest
      } else if (currentDir.endsWith("/")) {
        currentDir + dest
      } else {
        currentDir + "/" + dest
      }
    }
  }

  def getDeepestDir(dest: Array[String], children: Set[INode]): Option[String] = {
    dest match {
      case d: Array[String] if d.isEmpty => None
      case d: Array[String] if(dest.size == 1 && children.filter(c => c.fileName.equals(dest.head)).filter(d => d.isDir).size == 1)
        => Some(dest.head)
      case d: Array[String] if (index.getOrElse(dest.head, null) != null) => getDeepestDir(dest drop 1, index.get(dest.head).get.children)
      case _ => None
    }
  }

  private def create(name: String, content: String): Unit = {
    val currentDirName = getCurrentDirName()
    if (currentDirName.children.filter(c => c.fileName.equals(name)).size > 0) {
      println("Filename already exists.")
      return
    }
    val file = new INode(name, false)
    file.content = content
    index.put(name, file)
    currentDirName.addChild(file)
  }

  private def mkdir(name: String): Unit = {
    val parentDir = getCurrentDirName()
    if (parentDir.children.filter(c => c.fileName.equals(name)).size > 0 ) {
      println("Directory already exists")
      return
    }
    val dir = new INode(name, true)
    parentDir.addChild(dir)
    index.put(name, dir)
    println("Directory " + name + " successfully created")
  }

  private def cat(fileName: String): Unit = {
    val children = getCurrentDirName().children
    val eq = children.filter(c => c.fileName.equals(fileName))
    if (eq.size !=1) {
      println("File " + fileName + " does not exist")
    } else {
      println(index.get(fileName).get.content)
    }
  }

  private def rm(fileName: String): Unit = {
    val parent = getCurrentDirName()
    val childFile = parent.children.filter(f => f.fileName.equals(fileName))
    if (childFile.isEmpty) {
      println("Filename " + fileName + " does not exist.")
    } else {
      parent.children = parent.children.filter(f => !f.fileName.equals(fileName))
    }
  }

  private def getCurrentDirName(): INode = {
    val parent = if(currentDir.equals("/")) currentDir else currentDir.split("/").last
    index.getOrElse(parent, null)
  }
}

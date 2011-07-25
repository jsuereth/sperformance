package sperformance
package util

import annotation.tailrec
import java.io._
import java.net.URL

object FileUtils {

  def ensureDirectoryExists(file : File) : Unit = {
    val dir = file.getParentFile
    if(!dir.exists()) {
      dir.mkdirs();
    }
  }

  /**
   * Obtains the relative path between a directory and a file
   */
  def relativePath(dir : File, file : File) : String = {
    lazy val dirPath = {
      val path = dir.getAbsolutePath
      if(!path.endsWith(File.separator)) path + File.separator else path
    }
    if(file.getAbsolutePath.startsWith(dirPath)) {
      file.getAbsolutePath.drop(dirPath.size)
    } else error(dir + " is not a parent of " + file)
  }

  /** Finds all the index.html files in a directory... */
  def findIndexes(dir : File) : List[File] = {
    def getDirsOrIndexes(dir : File) : List[File] = dir.listFiles(new FileFilter {
             def accept(path : File)  = path.isDirectory || path.getName.endsWith("index.html")
        }).toList
    def isIndexFile(f : File) = f.getPath.endsWith("index.html")

    @tailrec
    def findIndexesHelper(curFile : Option[File], filesToSearch : List[File], indexFiles : List[File]) : List[File] = curFile match {
      case None if filesToSearch.isEmpty =>
        indexFiles
      case None =>
        findIndexesHelper(filesToSearch.headOption, filesToSearch.tail, indexFiles)
      case Some(f) if f.isDirectory =>
        val nextFiles = getDirsOrIndexes(f) ++ filesToSearch
        findIndexesHelper(nextFiles.headOption, nextFiles.tail, indexFiles)
      case Some(f) => //Has to be index file
        findIndexesHelper(None, filesToSearch, f :: indexFiles)
    }
    findIndexesHelper(None, getDirsOrIndexes(dir), Nil)
  }

  val copy = org.apache.commons.io.FileUtils.copyURLToFile(_:URL,_:File)
  
  def writer[U](file:File)(f:Writer => U):U = manage(new OutputStreamWriter(new FileOutputStream(file), "UTF-8"))(f)

  def outputStream[U](file:File)(f:FileOutputStream => U):U = manage(new FileOutputStream(file))(f)

  def manage[U,R <: Closeable](c : R)(f:R => U):U = {
    try {
      f(c)
    } finally {
      c.close()
    }
  }
}
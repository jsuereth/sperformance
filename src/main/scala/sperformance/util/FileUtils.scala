package sperformance
package util

import annotation.tailrec
import java.io.{FileFilter, File, BufferedWriter}

object FileUtils {

  def withBufferedWriterFor[A](file : File)(f : BufferedWriter => A) = {
    ensureParentExists(file)
    val output = new BufferedWriter(new java.io.FileWriter(file))
    try {
      f(output)
    } finally {
      output.close();
    }
  }

  def ensureParentExists(file : File) : Unit = {
    val dir = file.getParentFile
    if(!dir.isDirectory) dir.mkdirs()
  }

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
}
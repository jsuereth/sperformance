package sperformance
package util

import java.io.File

object FileUtils {
  def relativePath(dir : File, file : File) : String = {
    lazy val dirPath = {
      val path = dir.getAbsolutePath
      if(!path.endsWith(File.separator)) path + File.separator else path
    }
    if(file.getAbsolutePath.startsWith(dirPath)) {
      file.getAbsolutePath.drop(dirPath.size)
    } else error(dir + " is not a parent of " + file)
  }
}
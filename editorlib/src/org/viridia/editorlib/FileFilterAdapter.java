package org.viridia.editorlib;

import java.io.File;
import java.io.FilenameFilter;

import javax.swing.filechooser.FileFilter;

/**
 * Adapts a FileFilter to a FilenameFilter.
 */
public class FileFilterAdapter implements FilenameFilter {
  public final FileFilter fileFilter;

  public FileFilterAdapter(FileFilter fileFilter) {
    this.fileFilter = fileFilter;
  }

  @Override
  public boolean accept(File dir, String filename) {
    return fileFilter.accept(new File(dir, filename));
  }
}

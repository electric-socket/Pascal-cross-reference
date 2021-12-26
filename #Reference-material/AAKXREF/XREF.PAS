{$C-,V+,R+,I+,U-,K+}  {* essential for programmed pause-abort facility;
                                  see procedure dealwithuser *}
program xrefpas ( INPUT , OUTPUT ) ;

{$I XREF.CST }
{$I DATETIME.CST }
{$I PRINTER.CST  }

{$I STRINGS.TYP  }
{$I REG.TYP      }
{$I DATETIME.TYP }
{$I XREF.TYP     }

{$I DATETIME.VAR  }
{$I XREF.VAR      }

(********************************* level 000 *****************************)
{$I BCDTOINT.FUN  }
{$I EXIST.FUN     }
{$I MEMORY.FUN    }
{$I VALDATEN.FUN  }
{$I REV_POS.FUN   }
{$I FILESEPR.PAS  }
{$I UPPERSTR.PAS  }
{$I A_LIN_CH.PAS }
{$I A_LIN_ST.PAS }
{$I DELPAGE.PAS  }
{$I PRNTOUT.PAS  }
(********************************* level 001 *****************************)
{$I TIMNTIM0.FUN  }
{$I FIXPATH.FUN   }
{$I DATNDAT0.PAS  }
{$I FDATNUMS.PAS  }
{$I READDATE.PAS  }
{$I READTIME.PAS  }
{$I N_Line.PAS   }
{$I N_Page.PAS   }
{$I MISCPROC.PAS }
{$I PRN_PAGE.PAS }
(********************************* level 002 *****************************)
{$I CURDATE0.PAS  }
{$I CURTIME0.PAS  }
{$I FTIMSTMP.PAS  }
{$I PRNTPAGE.PAS }
(********************************* level 003 *****************************)
{$I NEWPAGE.PAS  }
(********************************* level 004 *****************************)
{$I NEW_LINE.PAS }
(********************************* level 005 *****************************)
{$I WRITEID.PAS  }
{$I STRTPARS.PAS }
{$I PRINTREE.PAS }
{$I DOLIST.PAS   }
{$I GETINFO.PAS  }
{$I MAIN.PAS     }

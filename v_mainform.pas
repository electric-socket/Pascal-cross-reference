unit v_mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Menus, StdCtrls;

type

  { TMain }

  TMain = class(TForm)
    CommentChar: TEdit;
    Label1: TLabel;
    ProgramBox: TGroupBox;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuIEdit: TMenuItem;
    MenuIEdirSettings: TMenuItem;
    Menu99: TMenuItem;
    MenuFileExit: TMenuItem;
    MenuEditListing: TMenuItem;
    MenuEditCref: TMenuItem;
    MenuEditLang: TMenuItem;
    MenuEditProg: TMenuItem;
    MenuOpen: TMenuItem;
    MenuOpenConfig: TMenuItem;
    MenuOpenSave: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure MenuEditProgClick(Sender: TObject);
    procedure MenuFileExitClick(Sender: TObject);
    procedure MenuIEdirSettingsClick(Sender: TObject);




  private

  public

  end;

var
  Main: TMain;
  Dirty: Boolean;

implementation

{$R *.lfm}

{ TMain }


procedure TMain.MenuIEdirSettingsClick(Sender: TObject);
begin

end;

procedure TMain.MenuFileExitClick(Sender: TObject);
begin
    If Dirty then
    begin


    end;
    if Not Dirty then
       Close

end;

procedure TMain.FormCreate(Sender: TObject);
begin

end;

procedure TMain.MenuEditProgClick(Sender: TObject);
begin

end;

end.


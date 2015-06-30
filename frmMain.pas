unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Comctrls, ExtCtrls, ImgList,
  ShellAPI, clipbrd, Menus, FileCtrl, FolderDialog;

type
  TMainForm = class(TForm)
    fdFolder: TFolderDialog;
    pnlStatus: TPanel;
    ilImages: TImageList;
    lblCopyright: TLabel;
    lblWeb: TLabel;
    pcMain: TPageControl;
    tsTree: TTabSheet;
    tvShell: TTreeView;
    sdDialog: TSaveDialog;
    gbStatistics: TGroupBox;
    pnlFolderCount: TLabel;
    pnlFilesCount: TLabel;
    pnlSizeMB: TLabel;
    gbTools: TGroupBox;
    btnExpandAll: TButton;
    btnCollapseAll: TButton;
    btnClear: TButton;
    btnDoTextTree: TButton;
    btnSaveAs: TButton;
    gbOptions: TGroupBox;
    chbFoldersOnly: TCheckBox;
    chbCount: TCheckBox;
    chbShowInfoPanels: TCheckBox;
    gbButtons: TGroupBox;
    btnPickUpFolder: TButton;
    btnCancel: TButton;
    btnRefresh: TButton;
    chbAskBeforeRun: TCheckBox;
    btnExplore: TButton;
    btnOpen: TButton;
    btnFileDelete: TButton;
    btnFolderChangeName: TButton;
    btnFileExecute: TButton;

    function GetFullPathForSelectedNode(): String;
    function GetFileSize(FileName: String): Int64;
    function GetSizeWithProperPrefix(Size: Int64): String;
    function ListujKatalogi(tnRodzic: TTreeNode; strPath: string): Int64;
    function MakePolishEnding(Value: Integer; sText1, sText2, sText5, sAdd1, sAdd2, sAdd5: String): String;

    procedure DrawTree(Folder: String);
    procedure ChangeElementsEnableState(State: Boolean);

    procedure btnPickUpFolderClick(Sender: TObject);
    procedure btnExpandAllClick(Sender: TObject);
    procedure btnCollapseAllClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure chbCountClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure chbAskBeforeRunClick(Sender: TObject);
    procedure lblWebClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure chbFoldersOnlyClick(Sender: TObject);
    procedure btnDoTextTreeClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure btnFolderChangeNameClick(Sender: TObject);
    procedure btnFileExecuteClick(Sender: TObject);
    procedure chbShowInfoPanelsClick(Sender: TObject);
    procedure btnFileDeleteClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure tvShellChange(Sender: TObject; Node: TTreeNode);
    procedure btnExploreClick(Sender: TObject);
  private
    { Private declarations }
  public
    fiCount, foCount, siCount: Int64;
  end;

var
  MainForm: TMainForm;
  GlobalCancel: Boolean;

implementation

{$R *.DFM}
{$R WinXP.res}

function TMainForm.GetFullPathForSelectedNode(): String;
var
        tnTemp: TTreeNode;
        sTemp, sPath: String;
begin
        Result := '';

        tnTemp := tvShell.Selected;
        sPath := tnTemp.Text;

        if Pos('(', sPath) > 0 then sPath := Copy(sPath, 1, Pos('(', sPath) - 2);
        if Pos('[', sPath) > 0 then sPath := Copy(sPath, 2, Pos(']', sPath) - 2);

        while tnTemp.Parent <> nil do
        begin
                tnTemp := tnTemp.Parent;
                sTemp := tnTemp.Text;

                if Pos('(', sTemp) > 0 then sTemp := Copy(sTemp, 1, Pos('(', sTemp) - 2);
                if Pos('[', sTemp) > 0 then sTemp := Copy(sTemp, 2, Pos(']', sTemp) - 2);

                sPath := sTemp + '\' + sPath;
        end;

        if Pos('Mój komputer\', sPath) > 0 then sPath := Copy(sPath, 14, Length(sPath));

        Result := sPath;
end;

function TMainForm.GetFileSize(FileName: String): Int64;
var
        SearchRec: TSearchRec;
begin
        if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0 then
        begin
                Result := SearchRec.Size;
                SysUtils.FindClose(SearchRec);
        end
        else Result := 0;
end;

function TMainForm.GetSizeWithProperPrefix(Size: Int64): String;
begin
        Result := IntToStr(Size) + ' B';

        if Size > 1024 then Result := IntToStr(Round(Size / 1024)) + ' kB';
        if Size > 1048576 then Result := FloatToStrF(Size / 1048576, ffFixed, 7, 2) + ' MB';
        if Size > 1073741824 then Result := FloatToStrF(Size / 1073741824, ffFixed, 7, 2) + ' GB';
end;

function TMainForm.ListujKatalogi(tnRodzic: TTreeNode; strPath: string): Int64;
var
        sr: TSearchRec;
        iSize, iWynik: Integer;
        tnDziecko: TTreeNode;
        sSize: String;
        DirSize, TotalSize: Int64;
begin
        pnlStatus.Caption := ' Adding: ' + strPath;
        Application.ProcessMessages;

        TotalSize := 0;

        strPath := IncludeTrailingBackslash(strPath);
        iWynik := FindFirst(strPath + '*.*', faAnyFile, sr);

        while iWynik = 0 do
        begin
                if GlobalCancel = True then
                begin
                        FindClose(sr);

                        if not chbFoldersOnly.Checked then
                        begin
                                pnlFilesCount.Caption := MakePolishEnding(fiCount, 'file', 'files', 'files', '', '', '');
                                pnlFolderCount.Caption := MakePolishEnding(foCount, 'folder', 'folders', 'folders', '', '', '');
                                pnlSizeMB.Caption := IntToStr(Round(siCount / 1048576)) + ' MB (' + FloatToStrF(siCount / 1073741824, ffFixed, 7, 2) + ' GB)';
                        end;

                        Result := TotalSize;
                        exit;
                end;

                if ((sr.Attr and faDirectory) = faDirectory) and (sr.Name <> '.') and (sr.Name <> '..') then
                begin
                        tnDziecko := tvShell.Items.AddChild(tnRodzic, '[' + sr.Name + ']');
                        tnDziecko.ImageIndex := 0;
                        tnDziecko.SelectedIndex := 0;
                        Inc(foCount);

                        DirSize := ListujKatalogi(tnDziecko, strPath + sr.Name + '\');
                        Inc(TotalSize, DirSize);

                        if chbCount.Checked then
                        begin
                                sSize := GetSizeWithProperPrefix(DirSize);
                                tnDziecko.Text := tnDziecko.Text + ' (' + sSize + ')'
                        end;
                end
                else
                begin
                        if (sr.Name <> '.') and (sr.Name <> '..') and (chbFoldersOnly.Checked <> True) then
                        begin
                                iSize := GetFileSize(strPath + sr.Name);
                                sSize := GetSizeWithProperPrefix(iSize);

                                Inc(TotalSize, iSize);

                                Inc(fiCount);
                                Inc(siCount, iSize);

                                if chbCount.Checked then
                                        tnDziecko := tvShell.Items.AddChild(tnRodzic, sr.Name + ' (' + sSize + ')')
                                else
                                        tnDziecko := tvShell.Items.AddChild(tnRodzic, sr.Name);

                                tnDziecko.ImageIndex := 1;
                                tnDziecko.SelectedIndex := 1;
                        end;
                end;

                iWynik := FindNext(sr);
        end;

        FindClose(sr);

        if not chbFoldersOnly.Checked then
        begin
                pnlFilesCount.Caption := MakePolishEnding(fiCount, 'file', 'files', 'files', '', '', '');
                pnlFolderCount.Caption := MakePolishEnding(foCount, 'folder', 'folders', 'folders', '', '', '');
                pnlSizeMB.Caption := IntToStr(Round(siCount / 1048576)) + ' MB (' + FloatToStrF(siCount / 1073741824, ffFixed, 7, 2) + ' GB)';

                Application.ProcessMessages;
        end;

        Result := TotalSize;
end;

procedure TMainForm.DrawTree(Folder: String);
var
        tnRoot, tnTemp: TTreeNode;
        Start: DWORD;
        iDrvType: Integer;
        cDrive: Char;
        sSize, sETBs: String;
        TotalSize, DirSize: Int64;
begin
        Screen.Cursor := crHourglass;
        Start := GetTickCount();

        ChangeElementsEnableState(False);
        btnCancel.Enabled := True;

        btnClearClick(self);
        tvShell.Items.BeginUpdate;

        TotalSize := 0;

        if Folder = '' then
        begin
                tnRoot := tvShell.Items.AddChild(nil, '[Mój komputer]');
                tnRoot.ImageIndex := 7;
                tnRoot.SelectedIndex := 7;

                for cDrive := 'a' to 'z' do
                begin
                        iDrvType := GetDriveType(PChar(cDrive + ':\'));
                        if (iDrvType <> 0) and (iDrvType <> 1) then
                        begin
                                tnTemp := tvShell.Items.AddChild(tnRoot, UpperCase(cDrive) + ':');
                                tnTemp.ImageIndex := iDrvType;
                                tnTemp.SelectedIndex := iDrvType;

                                DirSize := ListujKatalogi(tnTemp, UpperCase(cDrive) + ':\');
                                Inc(TotalSize, DirSize);

                                if chbCount.Checked then
                                begin
                                        sSize := GetSizeWithProperPrefix(DirSize);
                                        tnTemp.Text := tnTemp.Text + ' (' + sSize + ')'
                                end;
                        end;
                end;

                if chbCount.Checked then
                begin
                        sSize := GetSizeWithProperPrefix(TotalSize);
                        tnRoot.Text := tnRoot.Text + ' (' + sSize + ')'
                end;
        end
        else
        begin
                sETBs := ExcludeTrailingBackslash(Folder);
                tnRoot := tvShell.Items.AddChild(nil, '[' + sETBs + ']');

                if sETBs = ExtractFileDrive(Folder) then
                begin
                        tnRoot.ImageIndex := GetDriveType(PChar(sETBs));
                        tnRoot.SelectedIndex := GetDriveType(PChar(sETBs));
                end
                else
                begin
                        tnRoot.ImageIndex := 0;
                        tnRoot.SelectedIndex := 0;
                end;

                DirSize := ListujKatalogi(tnRoot, Folder);

                if chbCount.Checked then
                begin
                        sSize := GetSizeWithProperPrefix(DirSize);
                        tnRoot.Text := tnRoot.Text + ' (' + sSize + ')'
                end;
        end;

        tvShell.AlphaSort();
        tnRoot.Expand(False);
        tvShell.Items.EndUpdate;

        ChangeElementsEnableState(True);
        btnCancel.Enabled := False;

        pnlStatus.Caption := ' Ready! Processing time: ' + IntToStr(GetTickCount() - Start) + ' ms.';
        Screen.Cursor := crDefault;
end;

procedure TMainForm.btnPickUpFolderClick(Sender: TObject);
begin
        if fdFolder.Directory = '' then fdFolder.Directory := ExtractFilePath(Application.ExeName);
        if not fdFolder.Execute then exit;

        GlobalCancel := False;
        DrawTree(fdFolder.Directory);
end;

procedure TMainForm.btnExpandAllClick(Sender: TObject);
var
        Start: DWORD;
begin
        if tvShell.Items.Count < 2 then exit;

        Screen.Cursor := crHourglass;
        Start := GetTickCount();

        ChangeElementsEnableState(False);
        
        pnlStatus.Caption := ' Expanding all tree nodes...';
        Application.ProcessMessages;

        tvShell.FullExpand;

        ChangeElementsEnableState(True);

        pnlStatus.Caption := ' Ready! Processing time: ' + IntToStr(GetTickCount() - Start) + ' ms.';
        Screen.Cursor := crDefault;
end;

procedure TMainForm.btnCollapseAllClick(Sender: TObject);
var
        Start: DWORD;
begin
        if tvShell.Items.Count < 2 then exit;
        
        Screen.Cursor := crHourglass;
        Start := GetTickCount();
        
        pnlStatus.Caption := ' Collapsing all tree nodes...';
        Application.ProcessMessages;

        ChangeElementsEnableState(False);

        tvShell.FullCollapse;

        tvShell.Items[0].Expand(False);

        ChangeElementsEnableState(True);

        pnlStatus.Caption := ' Ready! Processing time: ' + IntToStr(GetTickCount() - Start) + ' ms.';
        Screen.Cursor := crDefault;
end;

procedure TMainForm.btnClearClick(Sender: TObject);
begin
        tvShell.Items.Clear;
        pnlStatus.Caption := ' Ready! ';

        fiCount := 0;
        foCount := 0;
        siCount := 0;
        if not chbFoldersOnly.Checked then
        begin
                pnlFilesCount.Caption := '0 files';
                pnlFolderCount.Caption := '0 folders';
                pnlSizeMB.Caption := IntToStr(Round(siCount / 1048576)) + ' MB (' + FloatToStrF(siCount / 1073741824, ffFixed, 7, 2) + ' GB)';
        end;

        btnDoTextTree.Enabled := False;
        btnSaveAs.Enabled := False;
        btnExpandAll.Enabled := False;
        btnCollapseAll.Enabled := False;
end;

procedure TMainForm.chbCountClick(Sender: TObject);
begin
        gbStatistics.Visible := chbCount.Checked;

        if (gbStatistics.Visible) and (not chbFoldersOnly.Checked) then
        begin
                pnlFilesCount.Caption := '0 files';
                pnlFolderCount.Caption := '0 folders';
                pnlSizeMB.Caption := IntToStr(Round(siCount / 1048576)) + ' MB (' + FloatToStrF(siCount / 1073741824, ffFixed, 7, 2) + ' GB)';
        end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
        GlobalCancel := False;
end;

procedure TMainForm.btnCancelClick(Sender: TObject);
begin
        GlobalCancel := True;
end;

procedure TMainForm.chbAskBeforeRunClick(Sender: TObject);
begin
        if not chbAskBeforeRun.Checked then if Application.MessageBox('When this checkbox is unchecked, any accidental press on "Delete" button will delete file without confirmation!'+chr(13)+'(Warning! All files are deleted WITHOUT using Recycle Bin -- this operation in permanent!)'+chr(13)+''+chr(13)+'Are you sure, you want to disable confirmations?','Warning!',MB_YESNO+MB_ICONWARNING+MB_DEFBUTTON2) = ID_NO then chbAskBeforeRun.Checked := True;
end;

procedure TMainForm.lblWebClick(Sender: TObject);
begin
        ShellExecute(Handle,'open','http://www.gaman.pl/','','',SW_SHOW);
end;

procedure TMainForm.btnRefreshClick(Sender: TObject);
begin
        GlobalCancel := False;
        DrawTree(fdFolder.Directory);
end;

procedure TMainForm.chbFoldersOnlyClick(Sender: TObject);
begin
        if chbFoldersOnly.Checked then
        begin
                pnlFilesCount.Enabled := False;
                pnlSizeMB.Enabled := False;
                pnlFilesCount.Caption := '--';
                pnlSizeMB.Caption := '--';
        end
        else
        begin
                pnlFilesCount.Enabled := True;
                pnlSizeMB.Enabled := True;
                pnlFilesCount.Caption := IntToStr(fiCount);
                pnlSizeMB.Caption := IntToStr(Round(siCount / 1048576)) + ' MB (' + FloatToStrF(siCount / 1073741824, ffFixed, 7, 2) + ' GB)';
        end;
end;

procedure TMainForm.btnDoTextTreeClick(Sender: TObject);
var
        Start: DWORD;
        clp: TClipboard;
        slTemp: TStringList;
begin
        if tvShell.Items.Count < 2 then exit;

        Screen.Cursor := crHourglass;
        Start := GetTickCount();

        pnlStatus.Caption := ' Copying tree contents to clipboard...';
        Application.ProcessMessages;

        ChangeElementsEnableState(False);

        try
                tvShell.SaveToFile('temp.txt');

                slTemp := TStringList.Create;
                clp := TClipboard.Create;

                slTemp.LoadFromFile('temp.txt');
                clp.AsText := slTemp.Text;

                clp.Free;
                slTemp.Free;

                DeleteFile('temp.txt');
        except
                Application.MessageBox('To complete this operation a temporary file must be created (and then deleted). However, during attempt of writing temporary file an error was occured and entire operation was rolled back.'+chr(13)+''+chr(13)+'Check, if folder, from where your ran this program is not write-protected and repeat this operation','Error!',MB_OK+MB_ICONWARNING+MB_DEFBUTTON1);
        end;

        ChangeElementsEnableState(True);

        pnlStatus.Caption := ' Ready! Processing time: ' + IntToStr(GetTickCount() - Start) + ' ms.';
        Screen.Cursor := crDefault;
end;

procedure TMainForm.btnSaveAsClick(Sender: TObject);
var
        Start: DWORD;
begin
        if not sdDialog.Execute then exit;

        Screen.Cursor := crHourglass;
        Start := GetTickCount();

        pnlStatus.Caption := ' Saving tree contents to a file...';
        Application.ProcessMessages;

        ChangeElementsEnableState(False);

        try
                tvShell.SaveToFile(sdDialog.FileName);
        except
                Application.MessageBox('There was an error when writing text file! Check, if destination folder is not write-protected?','Error!',MB_OK+MB_ICONWARNING+MB_DEFBUTTON1);
        end;

        ChangeElementsEnableState(True);

        pnlStatus.Caption := ' Ready! Processing time: ' + IntToStr(GetTickCount() - Start) + ' ms.';
        Screen.Cursor := crDefault;
end;

procedure TMainForm.btnFolderChangeNameClick(Sender: TObject);
var
        sSize, sPath, sSource, sDest: String;
        iSize: Integer;
begin
        if tvShell.Selected = nil then exit;

        sPath := ExtractFilePath(GetFullPathForSelectedNode());
        sSource := ExtractFileName(GetFullPathForSelectedNode());
        sDest := InputBox('Rename', 'Enter new name for selected file:', sSource);

        if sDest = sSource then exit;

        if chbAskBeforeRun.Checked then if Application.MessageBox(PChar('Current name: ' + sSource + chr(13) + 'New name: ' + sDest + chr(13) + chr(13) + 'Folder: ' + sPath + chr(13) + chr(13) + 'Are you sure, you want to rename this file?'),'Rename...',MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = ID_NO then exit;

        if RenameFile(sPath + sSource, sPath + sDest) then
        begin
                iSize := GetFileSize(sPath + sDest);
                sSize := GetSizeWithProperPrefix(iSize);

                tvShell.Selected.Text := sDest + ' (' + sSize + ')';
        end
        else Application.MessageBox('Rename of selected file failed!','Error...',MB_OK+MB_ICONWARNING+MB_DEFBUTTON1);

        tvShell.SetFocus;
end;

procedure TMainForm.btnFileExecuteClick(Sender: TObject);
var
        sPath: String;
begin
        if tvShell.Selected = nil then exit;

        sPath := GetFullPathForSelectedNode();
        if not FileExists(sPath) then exit;

        if chbAskBeforeRun.Checked then
        begin
                if ExtractFileExt(sPath) = '.exe' then
                begin
                        if Application.MessageBox(PChar('Do you really want to run program:'+chr(13)+sPath+chr(13)+chr(13)+'Certain files may contain viruses or become in any other way potentially dangerous to your computer!'),'Execute program...',MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = ID_NO then exit;

                        ShellExecute(Handle, 'open', PChar(sPath), '', '', SW_SHOW);
                end
                else
                begin
                        if Application.MessageBox(PChar('Do you really want to execute this file:'+chr(13)+sPath+chr(13)+chr(13)+'A program set in Windows to handle this kind of files will be launched in order to exectute selected files'),'Execite file...',MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = ID_NO then exit;

                        ShellExecute(Handle, 'open', PChar(sPath), '', '', SW_SHOW);
                end;
        end
        else ShellExecute(Handle, 'open', PChar(sPath), '', '', SW_SHOW);

        tvShell.SetFocus();
end;

procedure TMainForm.chbShowInfoPanelsClick(Sender: TObject);
begin
        gbTools.Visible := chbShowInfoPanels.Checked;
end;

function TMainForm.MakePolishEnding(Value: Integer; sText1, sText2, sText5, sAdd1, sAdd2, sAdd5: String): String;
var
        iResult, iLast, iLast_Two: Integer;
        sBeginAdd, sBaseText, sValue: String;
begin
     iResult := 3;

     sValue := IntToStr(Value);

     iLast := StrToIntDef(Copy(sValue, Length(sValue), 1), 0);
     iLast_Two := StrToIntDef(Copy(sValue, Length(sValue) - 1, 2), 0);

     if ((iLast > 1) and (iLast < 5)) then iResult := 2;

     if ((iLast_Two > 10) and (iLast_Two < 20)) then iResult := 3;

     if (Value = 1) then iResult := 1;

     sBeginAdd := '';

     if (iResult = 1) then
     begin
	  sBaseText := sText1;
	  if (sAdd1 <> '') then sBeginAdd := sAdd1 + ' ';
     end;
     
     if (iResult = 2) then
     begin
	  sBaseText := sText2;
	  if (sAdd2 <> '') then sBeginAdd := sAdd2 + ' ';
     end;
     
     if (iResult = 3) then
     begin
	  sBaseText := sText5;
	  if (sAdd5 <> '') then sBeginAdd := sAdd5 + ' ';
     end;
	
     Result := sBeginAdd + IntToStr(Value) + ' ' + sBaseText;
end;

procedure TMainForm.ChangeElementsEnableState(State: Boolean);
begin
        btnPickUpFolder.Enabled := State;
        btnExpandAll.Enabled := State;
        btnCollapseAll.Enabled := State;
        btnClear.Enabled := State;
        chbCount.Enabled := State;
        chbShowInfoPanels.Enabled := State;
        chbAskBeforeRun.Enabled := State;
        btnDoTextTree.Enabled := State;
        btnSaveAs.Enabled := State;
        btnRefresh.Enabled := State;
        chbFoldersOnly.Enabled := State;
end;

procedure TMainForm.btnFileDeleteClick(Sender: TObject);
var
        sFile: String;
begin
        if tvShell.Selected = nil then exit;

        sFile := GetFullPathForSelectedNode();

        if chbAskBeforeRun.Checked then if Application.MessageBox(PChar('Warning! This file will be PERMANENTLY deleted:' + chr(13) + chr(13) + sFile + chr(13) + chr(13) + 'Are you really, REALLY sure, you want to delete this file?' + chr(13) + '(files are deleted WITHOUT using Windows Recycle Bin and this operation CAN NOT BE UNDONE!)'),'Delete file...',MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = ID_NO then exit;

        if not DeleteFile(sFile) then
                Application.MessageBox('File deletion failed!','B³¹d...',MB_OK+MB_ICONWARNING+MB_DEFBUTTON1)
        else
                tvShell.Selected.Delete;

        tvShell.SetFocus();
end;

procedure TMainForm.btnOpenClick(Sender: TObject);
var
        sPath: String;
begin
        if tvShell.Selected = nil then exit;

        if tvShell.Selected.ImageIndex <> 1 then
                sPath := ExtractFilePath(IncludeTrailingBackslash(GetFullPathForSelectedNode()))
        else
                sPath := ExtractFilePath(GetFullPathForSelectedNode());

        if not DirectoryExists(sPath) then exit;

        ShellExecute(Handle, 'open', PChar(sPath), '', '', SW_SHOW);

        tvShell.SetFocus();
end;

procedure TMainForm.tvShellChange(Sender: TObject; Node: TTreeNode);
begin
        if tvShell.Selected = nil then
        begin
                btnFileExecute.Enabled := False;
                btnFolderChangeName.Enabled := False;
                btnFileDelete.Enabled := False;
                btnOpen.Enabled := False;
                btnExplore.Enabled := False;
                exit;
        end
        else
        begin

                btnFileExecute.Enabled := (tvShell.Selected.ImageIndex = 1);
                btnFolderChangeName.Enabled := (tvShell.Selected.ImageIndex = 1);
                btnFileDelete.Enabled := (tvShell.Selected.ImageIndex = 1);

                //btnOpen.Enabled := (tvShell.Selected.ImageIndex <> 1);
                btnOpen.Enabled := True;
                //btnExplore.Enabled := (tvShell.Selected.ImageIndex <> 1);
                btnExplore.Enabled := True;
        end;
end;

procedure TMainForm.btnExploreClick(Sender: TObject);
var
        sPath: String;
begin
        if tvShell.Selected = nil then exit;

        if tvShell.Selected.ImageIndex <> 1 then
                sPath := ExtractFilePath(IncludeTrailingBackslash(GetFullPathForSelectedNode()))
        else
                sPath := ExtractFilePath(GetFullPathForSelectedNode());
                
        if not DirectoryExists(sPath) then exit;

        ShellExecute(Handle, 'explore', PChar(sPath), '', '', SW_SHOW);

        tvShell.SetFocus();
end;

end.

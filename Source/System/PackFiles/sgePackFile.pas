{
Пакет             Simple Game Engine 2
Файл              sgePackFile.pas
Версия            1.0
Создан            08.05.2021
Автор             Творческий человек  (accuratealx@gmail.com)
Описание          Заголовки и функции работы с архивами
}
{$Include Defines.inc}

unit sgePackFile;

{$mode objfpc}{$H+}

interface



type
  //Заголовок файла
  TsgePackFileHeader = packed record
    hLabel: array[0..6] of Byte;                                    //7b Метка
    hVersion: Byte;                                                 //1b Версия архива
  end;


  //Заголовок блока
  TsgePackFileBlock = packed record
    TotalSize: Cardinal;                                            //4b Размер блока
    NameSize: Word;                                                 //2b Длина имени файла
  end;



function sgePackFile_GetFileHead: TsgePackFileHeader;


implementation


function sgePackFile_GetFileHead: TsgePackFileHeader;
const
  PackLabel = 'SGEPACK';
begin
  Move(PackLabel, Result.hLabel, SizeOf(PackLabel));
  Result.hVersion := 1;
end;


end.


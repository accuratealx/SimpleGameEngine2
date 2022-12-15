//Определения
{$Define SGE_DEBUG}       //Режим отладки
//{$Define SGE_GUI_Bounds}  //Вывод рамки GUI елементов


//Режим отладки
{$IfDef SGE_DEBUG}
  {$Warnings On}
  {$Hints On}
{$Else}
  {$Warnings Off}
  {$Hints Off}
{$EndIf}


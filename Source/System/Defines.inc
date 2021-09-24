//Определения
{$Define SGE_DEBUG}



//Режим отладки
{$IfDef SGE_DEBUG}
  {$Warnings On}
  {$Hints On}
{$Else}
  {$Warnings Off}
  {$Hints Off}
{$EndIf}


//Вывод рамки GUI елементов
{$Define SGE_GUI_Bounds}

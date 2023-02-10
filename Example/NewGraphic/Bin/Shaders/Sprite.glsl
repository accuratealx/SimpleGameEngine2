#version 400 core

layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aTexCoord;

out vec2 TexCoord;               //Координаты тектуры

uniform vec2 ScreenSize;        //Размеры экрана
uniform vec3 Layer;             //Положение слоя - xy, масштаб - z
uniform vec2 Pos;               //Положение объекта на экране
uniform vec3 ScaleAngleAlpha;   //Масштаб, Угол поворота, прозрачность

//Нормализация координаты
vec2 ScreenPointToGLPoint(vec2 ScreenPoint)
{
    vec2 kwh = 2.0 /ScreenSize;
    return vec2(ScreenPoint.x, ScreenSize.y - ScreenPoint.y) * kwh - 1.0;
}

//Поворот точки
#define PI 3.1415926538
vec2 RotatePoint(vec2 Point, float Angle)
{
    //Перевод в радианы
    float a = Angle * PI / 180;
    float sinA = sin(a);
    float cosA = cos(a);
    return vec2(Point.x * cosA - Point.y * sinA, Point.x * sinA + Point.y * cosA);
}

void main()
{
    vec2 RealPoint = aPos;

    //Повернуть на угол
    if (ScaleAngleAlpha.y != 0)
    {
        RealPoint = RotatePoint(RealPoint, ScaleAngleAlpha.y);
    }

    //Поправить координаты относительно слоя
    RealPoint = RealPoint * Layer.z * ScaleAngleAlpha.x + Layer.xy + Pos;

    //Нормальзовать координаты
    vec2 GLPoint = ScreenPointToGLPoint(RealPoint);
    gl_Position = vec4(GLPoint.x, GLPoint.y, 0.0, 1.0);

    //Координаты вершины
    TexCoord = aTexCoord;    
}


PROGRAM_SEPARATOR


#version 400 core

out vec4 FragColor;             //Выходной цвет
in vec2 TexCoord;               //Координаты текстуры   

uniform vec3 ScaleAngleAlpha;   //Масштаб, Угол поворота, прозрачность
uniform sampler2D Texture0;     //Текстура 0

void main()
{
    FragColor = texture(Texture0, TexCoord) * vec4(1, 1, 1, ScaleAngleAlpha.z);
}
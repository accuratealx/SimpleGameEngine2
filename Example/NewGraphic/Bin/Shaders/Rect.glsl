#version 400 core

layout (location = 0) in vec2 aPos;

uniform vec2 ScreenSize;    //Размеры экрана
uniform vec3 Layer;         //Положение слоя - xy, масштаб - z
uniform vec2 Pos;           //Положение объекта на экране

//Нормализация координаты
vec2 ScreenPointToGLPoint(vec2 ScreenPoint)
{
    //Коэффициенты
    float kw = (2.0 / ScreenSize.x);
    float kh = (2.0 / ScreenSize.y);
    float x = ScreenPoint.x * kw - 1.0;
    float y = (ScreenSize.y - ScreenPoint.y) * kh - 1.0;
    return vec2(x, y);
}

void main()
{
    //Поправить координаты относительно слоя
    vec2 RealPoint;
    RealPoint.x = (aPos.x * Layer.z) + Layer.x + Pos.x;
    RealPoint.y = (aPos.y * Layer.z) + Layer.y + Pos.y;

    //Нормальзовать координаты
    vec2 GLPoint = ScreenPointToGLPoint(RealPoint);
    gl_Position = vec4(GLPoint.x, GLPoint.y, 0.0, 1.0);
}


PROGRAM_SEPARATOR


#version 400 core

out vec4 FragColor;

uniform vec4 Color; //Цвет примитива

void main()
{
    FragColor = Color;
}
#version 450 core

layout (location = 0) in vec2 aPos;

uniform vec2 ScreenSize;        //Размеры экрана
uniform vec4 Layer;             //Положение слоя - xy, масштаб - zw
uniform vec2 Pos;               //Положение объекта на экране
uniform vec2 Scale;             //Масштаб xy
uniform vec2 Origin;            //Точка вывода и поворота
uniform float Angle;            //Угол поворота

//Нормализация координаты
vec2 ScreenPointToGLPoint(vec2 ScreenPoint) {
	vec2 kwh = 2.0 / ScreenSize;
    return vec2(ScreenPoint.x, ScreenSize.y - ScreenPoint.y) * kwh - 1.0;
}

//Поворот точки
vec2 RotatePoint(vec2 Point, float angle) {
    //Перевод в радианы
    float sinA = sin(angle);
    float cosA = cos(angle);
    return vec2(Point.x * cosA - Point.y * sinA, Point.x * sinA + Point.y * cosA);
}

void main() {
    //Координаты вершины
    vec2 RealPoint = (aPos.xy + Origin.xy) * Scale.xy;

    //Повернуть на угол
    if (Angle != 0) {
        RealPoint = RotatePoint(RealPoint, Angle);
    }

    //Поправить координаты относительно слоя
    RealPoint = RealPoint * Layer.zw + Layer.xy + Pos;

    //Нормальзовать координаты
    vec2 GLPoint = ScreenPointToGLPoint(RealPoint);
    gl_Position = vec4(GLPoint, 0.0, 1.0);
}


PROGRAM_SEPARATOR


#version 450 core

out vec4 FragColor;
uniform vec4 Color;

void main() {
    FragColor = Color;
}
#version 400 core

layout (location = 0) in vec2 aPos;

void main()
{
    gl_Position = vec4(aPos, 0.0, 1.0);
}


PROGRAM_SEPARATOR


#version 400 core

out vec4 FragColor;
uniform vec4 Color;

void main()
{
    FragColor = Color;
}
#version 400 core

layout (location = 0) in vec2 aPos;

const vec4 points[] = vec4[]
(
    vec4(-1.0, 1.0, 0.0, 1.0),
    vec4(-1.0, -1.0, 0.0, 1.0),
    vec4(1.0, -1.0, 0.0, 1.0),
    vec4(-1.0, 1.0, 0.0, 1.0),
    vec4(1.0, -1.0, 0.0, 1.0),
    vec4(1.0,  1.0, 0.0, 1.0)
);

void main()
{
    gl_Position = points[gl_VertexID];
}


PROGRAM_SEPARATOR


#version 400 core

out vec4 FragColor;
uniform vec4 Color;

void main()
{
    FragColor = Color;
}
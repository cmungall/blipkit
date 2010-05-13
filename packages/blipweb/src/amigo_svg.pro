

sdefun(svg_body(ID)/'generates svg tag',
       svg:svg(height=10,width=10,xmlns='http://www.w3.org/2000/svg',xmlns:xlink='http://www.w3.org/1999/xlink',
               findall(svg:line(id=id,
                                style='fill: rgb(224,255,255); fill-opacity: 1.0; stroke: rgb(224,255,255); stroke-opacity: 1.0; stroke-width: 1',
                                x1=24,
                                x2=24,
                                y1=0,
                                y2=312)))).


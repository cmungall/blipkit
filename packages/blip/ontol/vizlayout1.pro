:- op(1100,xfy,::).
:- op(800,xfy,in).

develops_from :: lower(subject) > lower(object) in x. % horizontal, R to L

preceded_by :: lower(subject) = lower(object) in y.      % same height
preceded_by :: upper(subject) = upper(object) in y.      % same height
preceded_by :: lower(subject) > upper(object) in x.  % earlier event left-of right, not overlapping


part_of :: lower(subject) < lower(object) in y. % arrange vertically, sub-parts lower
part_of :: lower(subject) >= lower(object) in x.
part_of :: upper(subject) =< upper(object) in x. % sub-part horizontally contained by super-part

start :: lower(subject) = lower(object) in x.  % vertically aligned

end :: upper(subject) = upper(object) in x.  % vertically aligned

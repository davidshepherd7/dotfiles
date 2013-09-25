Config
{
font = "-*-Fixed-Bold-R-Normal-*-13-*-*-*-*-*-*-*",
bgColor = "black",
fgColor = "grey",
position = Top,
-- , lowerOnStart = False,
-- , hideOnStart = False,

-- For some reason this breaks things, possibly related to the gnome compat
-- stuff?

commands = [ Run MultiCpu ["-L","3","-H","50","--normal",
                           "green","--high","red",
                           "-t", "Cpus: <user0> <user1> <user2> <user3>"
                            ] 1

         , Run Memory ["-t","Mem: <usedratio>%"] 10
         , Run Date " %R %A %d %B %Y" "date" 10
         , Run StdinReader
         ],
sepChar = "%",
alignSep = "}{",
template = "     %StdinReader% }{ %multicpu% | %memory%    <fc=#ee9a00>%date%</fc> "
}

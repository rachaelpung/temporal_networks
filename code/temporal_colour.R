# colour
# col_hue_bmj = c('#00468B','#ED0000','#42B540','#0099B4','#925E9F') # BMJ
# col_hue_lancet = c('#00468BFF','#FDAF91FF','#42B540FF','#0099B4FF','#925E9FFF') # Lancet Dark
# col_hue_lancet = c('#9CD6E9', '#E0A6A1', '#C9E0B4', '#039FC6', '#9FADD4') # Lancet Pastel - blue, pink, green, turquiose, purple
# col_hue_lancet = c('#9FADD4', '#E0A6A1', '#C9E0B4', '#B09C85', '#9CD6E9') # replace turquoise with brown
# col_hue_nature = c('#E64B35FF', '#4DBBD5FF', '#00A087FF', '#3C5488FF', '#F39B7FFF', 
#                    '#8491B4FF', '#91D1C2FF', '#DC0000FF', '#7E6148FF', '#B09C85FF') # Nature

col_hue_lancet = c('#20639B', '#F6D55C',  '#3CAEA3', '#ED553B','#B09C85')

lightup = function(c, alpha)
{
  z=col2rgb(c)/255
  return(rgb(z[1],z[2],z[3],alpha))  # 0.125 for col3
}

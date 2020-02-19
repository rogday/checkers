library(plotrix)


bounds <- function(x){
  if (x <= 8 && x >= 1)
    return(T)
  return(F)
}

empty <- function(x, y){
  if (field[x, y] == 0)
    return(T)
  return(F)
}

kill <- function(x1, y1, x, y){
  if (field[x1, y1] * field[x, y] < 0)
    return(T)
  return(F)
}

checker <- function(x, y){
  if (abs(field[x, y]) == 1)
    return(T)
  return(F)
}


allow <- function(){
  list <- c()
  is_attack <<- 0
  
  if (checker(xf, yf)){
    fsign <- field[xf, yf]
    tx    <- xf - fsign
    tx2   <- xf + fsign
    ty    <- yf - fsign
    ty2   <- yf + fsign
    
    t2x   <- tx  - fsign
    t2x2  <- tx2 + fsign
    t2y   <- ty  - fsign
    t2y2  <- ty2 + fsign
    #backward
    if (bounds(t2x2)){
      if (bounds(t2y)){
        if (kill(tx2, ty, xf, yf) && empty(t2x2, t2y)){
          list <- c(list, t2x2, t2y)
          is_attack <<- 1
        }
      }
      if (bounds(t2y2) ){
        if (kill(tx2, ty2, xf, yf) && empty(t2x2, t2y2)){
          list <- c(list, t2x2, t2y2)
          is_attack <<- 1
        }
      }
    }
    
    #forward
    if (bounds(tx)){
      if  (bounds(ty)){
        if (empty(tx, ty) && is_attack == 0)
          list = c(list, tx, ty)
        else
          if (kill(tx, ty, xf, yf) && bounds(t2x) && bounds(t2y) && empty(t2x, t2y)){
            if (is_attack == 0){
              list <- c(t2x, t2y)
              is_attack <<- 1
            }else
              list <- c(list, t2x, t2y)
          }
      }
      
      if (bounds(ty2)){
        if (empty(tx, ty2) && is_attack == 0)
          list = c(list, tx, ty2)
        else
          if (kill(tx, ty2, xf, yf) && bounds(t2x) && bounds(t2y2) && empty(t2x, t2y2)){
            if (is_attack == 0){
              is_attack <<- 1
              list <- c(t2x, t2y2)
            }else
              list <- c(list, t2x, t2y2)
          }
      }
    }
    return(list)
  }
  
  
  lt_x <- max(xf, yf) - yf + 1
  lt_y <- max(xf, yf) - xf + 1
  
  rb_x <- xf + min(8 - xf, 8 - yf)
  rb_y <- yf + min(8 - xf, 8 - yf)
  
  lb_x <- xf + min(8 - xf, yf - 1)
  lb_y <- yf - min(8 - xf, yf - 1)
  
  rt_x <- xf - min(xf - 1, 8 - yf)
  rt_y <- yf + min(xf - 1, 8 - yf)
  
  #ways: x,y -> lt; x,y ->rb; x,y -> lb;  x,y -> rt
  list_cor <- c(lt_x, lt_y, rb_x, rb_y, lb_x, lb_y, rt_x, rt_y)
  dim(list_cor) <- c(2, 4)
  list_tmp <- c()
  make_list_tmp_again <- list()
  
  for (j in 1:4){
    tmp_x <- (xf:list_cor[,j][1])[-1]
    tmp_y <- (yf:list_cor[,j][2])[-1]
    enemies <- 0
    if (length(tmp_x) >= 1)
      for (i in 1:length(tmp_x)){
        if (enemies > 1)
          break
        if (empty(tmp_x[i], tmp_y[i]))
          list_tmp <- c(list_tmp, tmp_x[i], tmp_y[i])
        else{
          if ((kill(tmp_x[i], tmp_y[i], xf ,yf) || enemies != 0) && (i + 1) <= length(tmp_x) && bounds(tmp_x[i + 1]) && bounds(tmp_y[i + 1]) && empty(tmp_x[i + 1], tmp_y[i + 1])){
            enemies <- enemies + 1
            is_attack <<- 1
            if (enemies == 1)
              list_tmp <- c()
          }else
            break
        }
      }
    make_list_tmp_again[[j]] <- list(enemies, list_tmp)
  }
  if (is_attack == 1)
    for (i in 1:4){
      if (make_list_tmp_again[[i]][[1]] != 0)
        list<-c(list, make_list_tmp_again[[i]][[2]])
    }
  else{
    for (i in 1:4){
      list <- c(make_list_tmp_again[[i]][[2]])
    }
  }
  #list <-c(is_attack, 0, list)
  #dim(list) <- c(2, length(list)/2)
  return(list)
}

draw_empty_field <- function(q = c()){
  if (player)
    replayPlot(white_field)
  else
    replayPlot(black_field)
  
  if (length(q) != 0){
    if (q[1] == 1){
      #ways highlight
      if (player)
        rect((yf - 1) * size, (xf - 1) * size, yf * size, xf * size, col = "#238D43")
      else
        rect((8 - yf) * size, (8 - xf) * size, (9 - yf) * size, (9 - xf) * size, col = "#238D43")
      
      q <- q[-1]
      if (length(q) != 0){
        dim(q) <- c(2, length(q)/2)
        if (player){
          rect((q[2,] - 1) * size, (q[1,] - 1) * size, q[2,] * size, q[1,] * size, col = "#98fb98" )
        }
        else{
          rect((8 - q[2,]) * size, (8 - q[1,]) * size, (9 - q[2,]) * size, (9 - q[1,]) * size, col = "#98fb98")
        }
      }
    }else{
      #checkers highlight
      q <- q[-1]
      if (length(q) != 0){
        dim(q) <- c(2, length(q)/2)
        if (player)
          rect((q[2,] - 1) * size, (q[1,] - 1) * size, q[2,] * size, q[1,] * size, col = "#002238" )
        else
          rect((8 - q[2,]) * size, (8 - q[1,]) * size, (9 - q[2,]) * size, (9 - q[1,]) * size, col = "#002238")
      }
    }
  }
}

draw_checkers_except <- function(list = c()){
  size4 <- size / 4
  size8 <- size / 8
  
  if (player == 1)
    for (j in 1:tlen){
      i <- thing[1, j]
      k <- thing[2, j]
      if (field[i, k] != 0){
        if (field[i, k] < 0)
          color <- "#FFFFFF"
        else
          color <- "#FF0000"
        if (!isinq(c(i, k), list)){
          draw.circle((k - .5) * size, (i - .5) * size, size4, col = color)
          if (abs(field[i, k]) == 2)
            draw.circle((k - .5) * size, (i - .5) * size, size8, col = "#FFFF00")
        }
      }
    }
  else
    for (j in 1:tlen){
      i <- thing[1, j]
      k <- thing[2, j]
      if (field[i, k] != 0){
        if (field[i, k] < 0)
          color <- "#FFFFFF"
        else
          color <- "#FF0000"
        if (!isinq(c(i, k), list)){
          draw.circle((8.5 - k) * size, (8.5 - i) * size, size4, col = color)
          if (abs(field[i, k]) == 2)
            draw.circle((8.5 - k) * size, (8.5 - i) * size, size8, col = "#FFFF00")
        }
      }
    }
}

check <- function(){
  if (!bounds(xf) || !bounds(yf)){
    print("out of bounds")
    return(F)
  }
  
  if (empty(xf, yf)){
    print("too empty") #inside
    return(F)
  }
  
  if (field[xf, yf] < 0 && player == 0 || field[xf, yf] > 0 && player == 1 ){
    print("chose right fig pls")
    return(F)
  }
  return(T)
}

move <- function(){
  
  if (field[xf, yf] < 0)
    color <- "#FFFFFF"
  else
    color <- "#FF0000"
  
  if (animate){
    if (xf < x)
      draw_x <- seq(xf, x, .2)
    else
      draw_x <- seq(xf, x, -.2)
    if (yf < y)
      draw_y <- seq(yf, y, .2)
    else
      draw_y <- seq(yf, y, -.2)
    
    draw_empty_field()
    draw_checkers_except(c(xf, yf))
    save_plot <- recordPlot()
    
    if (player == 1)
      for (k in 1:length(draw_x)){
        replayPlot(save_plot)
        
        draw.circle((draw_y[k] - .5) * size, (draw_x[k] - .5) * size, size / 4, col = color)
        if (abs(field[xf, yf]) == 2)
          draw.circle((draw_y[k] - .5) * size, (draw_x[k] - .5) * size, size / 8, col = "#FFFF00")
        
        Sys.sleep(.1)
      }
    else
      for (k in 1:length(draw_x)){
        replayPlot(save_plot)
        
        draw.circle((8.5 - draw_y[k]) * size, (8.5 - draw_x[k]) * size, size / 4, col = color)
        if (abs(field[xf, yf]) == 2)
          draw.circle((8.5 - draw_y[k]) * size, (8.5 - draw_x[k]) * size, size / 8, col = "#FFFF00")
        
        Sys.sleep(.1)
      }
  }
  if (field[xf, yf] == 1 && (x == 1) || field[xf, yf] == -1 && (x == 8))
    field[x, y] <<- 2 * field[xf, yf]
  else
    field[x, y] <<- field[xf, yf]
  
  field[xf, yf] <<- 0
  
  vec_i <- (x:xf)[-c(1, length(x:xf))]
  vec_k <- (y:yf)[-c(1, length(x:xf))]
  
  if (length(vec_i) != 0)
    for (i in 1:length(vec_i)){
      if (field[vec_i[i], vec_k[i]] != 0){
        if (field[vec_i[i], vec_k[i]] > 0)
          fig_b <<- fig_b - 1
        else
          fig_w <<- fig_w - 1
        field[vec_i[i], vec_k[i]] <<- 0
      }
    }
  
  xf <<- x
  yf <<- y
}

isinq <- function(b, q){
  if (length(q) != 0){
    dim(q) <- c(2, length(q) / 2)
    for (i in 1:dim(q)[2]){
      if (b[1] == q[1, i] && b[2] == q[2, i])
        return(T)
    }
  }
  return(F)
}

input <-function(){
  dot <<- locator(n = 1)
  x <<- trunc(dot$y / size + 1)
  y <<- trunc(dot$x / size + 1)
  
  if (player == 0){
    x <<- 9 - x
    y <<- 9 - y
  }
}

all_attack <- function(){
  txf <- xf
  tyf <- yf
  tis_attack <- is_attack
  
  answer <- 0
  attackers_list <- c()
  moovers_list <- c()
  
  if (player){
    for (j in 1:tlen){
      i <- thing[1, j]
      k <- thing[2, j]
      
      if (field[i,k]<0){
        xf <<- i
        yf <<- k
        f <- allow()
        
        if (answer == 0 && length(f) != 0)
          moovers_list <- c(moovers_list, i, k)
        
        if (is_attack){
          answer <- 1
          attackers_list <- c(attackers_list, i ,k)
        }
        
      }
    }
  }else{
    for (j in 1:tlen){
      i <- thing[1, j]
      k <- thing[2, j]
      
      if (field[i,k]>0){
        xf <<- i
        yf <<- k
        f <- allow()
        
        if (answer == 0 && length(f) != 0)
          moovers_list <- c(moovers_list, i, k)
        
        if (is_attack){
          answer <- 1
          attackers_list <- c(attackers_list, i ,k)
        }
      }
    }
  }
  xf <<- txf
  yf <<- tyf
  is_attack <<- tis_attack
  
  if (answer == 0)
    return(list(F, moovers_list))
  else
    return(list(T, attackers_list))
}

######################################HEAD_START##################################################
size <- 30
animate <- 0
player <- 1
is_attack <- 1
fig_w <- 12
fig_b <- 12
field <- matrix(seq(0, 0, 8*8), 8, 8)
xf <- 0
yf <- 0

#########TRUE_PROGRAMMING_OPTIMIZATION###########
thing <- c()
for (i in 1:8){
  for (k in 1:8){
    if ((i + k) %% 2 == 0){
      thing <- c(thing, i, k)
    }
  }
}
dim(thing) <- c(2, length(thing) / 2)
tlen <- length(thing) / 2
#########FIRST PLOT###########
par(mar = c(0, 0, 0, 0))
plot(c(0, 0), c(1, 1), xlim = c(-size / 2, size * 8.5), ylim = c(-size / 2, size * 8.5), asp = 1, axes = F, ann = F, type = "n")

rect(0, 0, size * 8, size * 8, col = "gray")
rect((thing[1,] - 1) * size, (thing[2,] - 1) * size, thing[1,] * size, thing[2,] * size, col = "#000000")
saved_field <- recordPlot()

for (i in 1:8){
  text(-size / 2, size * (i - 0.5), toString(i))
  text(size * (i - 0.5), -size / 2, intToUtf8(64 + i))
}
white_field <- recordPlot()
replayPlot(saved_field)

for(i in 1:8){
  text(size * 8.5, size * (i - 0.5), toString(9 - i))
  text(size * (i - 0.5), size * 8.5, intToUtf8(73 - i))
}
black_field <- recordPlot()
#white > 0, black < 0
for (i in 1:8){
  for (k in 1:3){
    if ((i + k) %% 2 == 0){
      field[k, i] = -1
      field[9 - k, 9 - i] = 1
    }
  }
}

########################################HEAD_END#########################################################

print("Enjoy!")
#########################################GAME_CYCLE###########################################################
while (fig_w && fig_b){
  
  ans <- all_attack()
  
  draw_empty_field(c(0, ans[[2]]))
  draw_checkers_except()
  
  dot <- locator(n = 1)
  xf <- trunc(dot$y/size + 1)
  yf <- trunc(dot$x/size + 1)
  if (length(dot) == 0)
    break
  
  if (player == 0){
    xf <- 9 - xf
    yf <- 9 - yf
  }

  if (!check())
    next
  
  #we have fig to move, start mooving
  q <- allow()
  is_attack <- 1
  step <- 0
  while (is_attack){
    step <- step + 1
    
    draw_empty_field(c(1, q))
    draw_checkers_except()
    
    input()
    if (step == 1)
      q<-allow()
    
    if (length(ans[[2]]) != 0){
      if (is_attack == 0 && ans[[1]] && step == 1){
        player <- (player + 1) %% 2
        break
      }
    }else{
      if (player){
        fig_w <- 0
        fig_b <- 1
      }else{
        fig_b <- 1
        fig_w <- 0
      }
      break
    }
    
    while (!isinq(c(x, y), q) &&  (x != xf || y != yf || step != 1) ){
      input()
      q<-allow()
    }
    if (x == xf && y == yf){
      player <- (player + 1) %% 2
      break
    }
    
    transform <- field[xf, yf]
    
    move()
    
    if (is_attack)
      q <- allow()
    
    if (transform != field[xf, yf])
      is_attack <- F
  }
  player <- (player + 1) %% 2
}

#########################################GAME_CYCLE_END###########################################################
draw_empty_field()
draw_checkers_except()

  if (fig_w > fig_b){
    print("Congratulations! White won.")
  }else{
    print("Congratulations! Black won.")
  }

print("Thanks for playing!")

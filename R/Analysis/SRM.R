###################################################################
#
# R source code providing social relations modeling for 
# network data. 
#
# Authors: Cassy Dorff and Michael D. Ward
# For more on the SRM in other software
# see http://davidakenny.net/srm/srm.htm
#
##################################################################




#Purpose: SRM function

dyads<-
function(x){
	# diag of x must be zero
	 if (dim(x)[1] - dim(x)[2] == 0) {
		n<-rd<-dim(x)[1]
		cd<-dim(x)[2]
		dim<-(dim(x))
		diag(x)<-0
		
	d1<-(n-1)
	d2<-(n-2)
	d3<-(n*(d2))
	d4<-((d1)*(d2))
	d5<-(((d1)*(d2)/(2))-1)
	
	x_r<-rowSums(x)/(d1)
	x_c<-colSums(x)/(d1)
	x_t<-sum(x)/(n*(d1))
		
	a_hat<-((d1^2)/(d3) * x_r) + (((d1)/(d3))*x_c) - ((d1)/(d2) *x_t) #actor i perceives others
	b_hat<-((d1^2)/(d3) * x_c) + (((d1)/(d3))*x_r) - ((d1)/(d2) * x_t) #actor i is perceieved 
	
	g_hat<-matrix(NA, dim(x)[1], dim(x)[2])
	diag(g_hat)<-0.0  # enforce diagonal equal to zero
	 
	q1<-NULL
	q2<-NULL
	
	s_2g<-matrix(NA, dim(x)[1],1, byrow=T) #dyadic variance
	s_gg<-matrix(NA, dim(x)[1], dim(x)[2], byrow=T) #dyadic covariance
	s_ab<-matrix(NA, dim(x)[1], dim(x)[2], byrow=T)
	s_2a<-matrix(NA, dim(x)[1],1, byrow=T) 
	s_2b<-matrix(NA, dim(x)[1],1, byrow=T)
	
	for (i in 1:dim(x)[1]){
				for (j in  1:dim(x)[2]){
				
				g_hat[i,j]<-x[i,j] - a_hat[i] - b_hat[j] - x_t
				diag(g_hat)<-0.0
				}
				}
				
				g_hatdiffs<-g_hat - t(g_hat) #all the differences
				diag(g_hatdiffs)<-0.0
				g_hatsums<-((g_hat + t(g_hat))/2) #dividing by 2 is in equation #a_hat sums twice as big as g_ij + h_ji /2 
				
				for (i in 1:dim(x)[1]){
					for (j in  1:dim(x)[2]){
					g_hatsums[i,j]<-g_hatsums[i,j]^2 #making squares
					g_hatdiffs[i,j]<-g_hatdiffs[i,j]^2
					
					}
					} #getting all individual squares
					
				diag(g_hatsums)<-0.0
				q1<-sum(g_hatsums) #summing squares of g_hat sum
				q2<-sum(g_hatdiffs)/2
			
				s_2g<-((q1/d5) + (q2/d4))/2 #took out *1/2
				s_gg<-((q1/d5) - (q2/d4))/2
				
				s_ab<-((sum(a_hat * b_hat))/(d1)) - ((s_gg*(d1))/(d3)) - (s_2g/(d3))
			
				s_2a<-((sum(a_hat^2)/(d1)) - ((s_2g*(d1))/(d3)) - ((s_gg)/(d3)))
			
				s_2b<-((sum(b_hat^2)/(d1)) - ((s_2g*(d1))/(d3)) - ((s_gg)/(d3)))
				
				a_hat_output<-as.matrix(a_hat, nrow=rd, ncol=1)
				rownames(a_hat_output)<-rownames(x)
				colnames(a_hat_output)<-list(c("actor effect for i"))

				b_hat_output<-as.matrix(b_hat, nrow=1, ncol=cd)
				rownames(b_hat_output)<-rownames(x)
				colnames(b_hat_output)<-list(c("partner effect for i"))
				
				g_hat_output<-as.matrix(g_hat, nrow=rd, ncol=cd) #relationship effect i->j
				rownames(g_hat_output)<-rownames(x)
				colnames(g_hat_output)<-rownames(x)
				
				if (sum(abs(x-t(x))) == 0){
					s_gg<- 1
					s_ab<- 1
					
				}
				
				return(list("actor.effect.i" = a_hat_output, "partner.effect.i" = b_hat_output, "unique.effect.ij"= g_hat_output,  "unique.variance"=s_2g, "relationship.covariance"=s_gg, "actor.variance" =s_2a, "partner.variance" = s_2b, "actor.partner.covariance" =s_ab, "rowmeans" = x_r, "colmeans" = x_c))
				}
			else {stop("matrix must be square") }
}

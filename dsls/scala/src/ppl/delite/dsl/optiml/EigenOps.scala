package ppl.delite.dsl.optiml

import ppl.delite.core.ops.DeliteOP_SingleTask
import ppl.delite.dsl.optiml.specialized.DoubleMatrix
import ppl.delite.core.appinclude._
import ppl.delite.core.Delite

object EigenOps {

  protected[delite] def eigenSolver(m: Matrix[Double]) = Delite.run(OP_eig(m.asInstanceOf[DoubleMatrix]))

  protected[delite] case class OP_eig(origMat: DoubleMatrix)
    extends DeliteOP_SingleTask[Vector[Matrix[Double]]](origMat){

    def task = {
      if(origMat.numRows != origMat.numCols){
        println("you must pass in a square matrix!")
        throw new IllegalArgumentException //TODO: figure out if this is right
      }
      var n = origMat.numRows
      var V = Matrix.zeros(n, n)
      var d = Vector.zeros(n)
      var e = Vector.zeros(n)
      //var V = new DoubleMatrixImpl(n,n)
      //var d = new DoubleVectorImpl(true, n)
      //var e = new DoubleVectorImpl(true, n)

      //check if symmetric
      var issymmetric = true
      var index : Int = 0
      while(issymmetric && index < n*n){
        val i = index % n
        val j = (index - i)/n
        if(origMat(i,j) != origMat(j,i)) issymmetric = false
        index = index + 1
      }

      //now, find eigenstuff
      if (issymmetric) {

            V = origMat.clone
            //for(i <- 0 until n)
              //for(j <- 0 until n)
                //V(i,j) = origMat(i,j)

           // Tridiagonalize
          // Symmetric Householder reduction to tridiagonal form.

          //  This is derived from the Algol procedures tred2 by
          //  Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
          //  Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
          //  Fortran subroutine in EISPACK.

            //for (int j = 0; j < n; j++) {
            for(j <- 0 until n){
               d(j) = V(n-1, j);
            }

            // Householder reduction to tridiagonal form.

            var i = n-1
            while(i > 0){
            //for (int i = n-1; i > 0; i--) {

               // Scale to avoid under/overflow.

               var scale = 0.0;
               var h = 0.0;

               //for (int k = 0; k < i; k++) {
               for(k <- 0 until i){
                  scale = scale + scala.math.abs(d(k));
               }
               if (scale == 0.0) {
                  e(i) = d(i-1);

                  //for (int j = 0; j < i; j++) {
                  for(j <- 0 until i){
                     d(j) = V(i-1, j);
                     V(i, j) = 0.0;
                     V(j, i) = 0.0;
                  }
               }
               else {

                  // Generate Householder vector.

                  //for (int k = 0; k < i; k++) {
                  for(k <- 0 until i){
                     d(k) /= scale;
                     h += d(k) * d(k);
                  }
                  var f = d(i-1);
                  var g = scala.math.sqrt(h);
                  if (f > 0) {
                     g = -g;
                  }
                  e(i) = scale * g;
                  h = h - f * g;
                  d(i-1) = f - g;

                  //for (int j = 0; j < i; j++) {
                  for(j <- 0 until i){
                     e(j) = 0.0;
                  }

                  // Apply similarity transformation to remaining columns.

                  //for (int j = 0; j < i; j++) {
                  for(j <- 0 until i){
                     f = d(j);
                     V(j, i) = f;
                     g = e(j) + V(j, j) * f;

                     //for (int k = j+1; k <= i-1; k++) {
                     for(k <- j+1 until i){
                        g += V(k, j) * d(k);
                        e(k) += V(k, j) * f;
                     }
                     e(j) = g;
                  }
                  f = 0.0;

                  //for (int j = 0; j < i; j++) {
                  for(j <- 0 until i){
                     e(j) /= h;
                     f += e(j) * d(j);
                  }
                  var hh = f / (h + h);

                  //for (int j = 0; j < i; j++) {
                  for(j <- 0 until i){
                     e(j) -= hh * d(j);
                  }

                  //for (int j = 0; j < i; j++) {
                  for(j <- 0 until i){
                     f = d(j);
                     g = e(j);

                     //for (int k = j; k <= i-1; k++) {
                     for(k <- j until i){
                        V(k, j) -= (f * e(k) + g * d(k));
                     }
                     d(j) = V(i-1, j);
                     V(i, j) = 0.0;
                  }
               }
               d(i) = h;

              i = i-1
            }

            // Accumulate transformations.

            //for (int i = 0; i < n-1; i++) {
            for(i <- 0 until (n-1)){
               V(n-1, i) = V(i, i);
               V(i, i) = 1.0;
               var h = d(i+1);
               if (h != 0.0) {

                  //for (int k = 0; k <= i; k++) {
                  for(k <- 0 until (i+1)){
                     d(k) = V(k, i+1) / h;
                  }

                  //for (int j = 0; j <= i; j++) {
                  for(j <- 0 until (i+1)){
                     var g = 0.0;

                     //for (int k = 0; k <= i; k++) {
                     for(k <- 0 until (i+1)){
                        g += V(k, i+1) * V(k, j);
                     }

                     //for (int k = 0; k <= i; k++) {
                     for(k <- 0 until (i+1)){
                        V(k, j) -= g * d(k);
                     }
                  }
               }

               //for (int k = 0; k <= i; k++) {
               for(k <- 0 until (i+1)){
                  V(k, i+1) = 0.0;
               }
            }
            //for (int j = 0; j < n; j++) {
            for(j <- 0 until n){
               d(j) = V(n-1, j);
               V(n-1, j) = 0.0;
            }
            V(n-1, n-1) = 1.0;
            e(0) = 0.0;




           // Diagonalize.
            // Symmetric tridiagonal QL algorithm.
           //  This is derived from the Algol procedures tql2, by
           //  Bowdler, Martin, Reinsch, and Wilkinson, Handbook for
           //  Auto. Comp., Vol.ii-Linear Algebra, and the corresponding
           //  Fortran subroutine in EISPACK.

            //for (int i = 1; i < n; i++) {
            for(i <- 1 until n){
               e(i-1) = e(i);
            }
            e(n-1) = 0.0;

            var f = 0.0;
            var tst1 = 0.0;
            var eps = scala.math.pow(2.0,-52.0);
            //for (int l = 0; l < n; l++) {
            for(l <- 0 until n){

               // Find small subdiagonal element

               tst1 = scala.math.max(tst1,scala.math.abs(d(l)) + scala.math.abs(e(l)));
               var m : Int = l;
               var isntfound = true
               while (m < n && isntfound) {
                  if (scala.math.abs(e(m)) <= eps*tst1) {
                     //break;
                     isntfound = false
                     m = m-1
                  }
                  m=m+1
               }

               // If m == l, d(l) is an eigenvalue,
               // otherwise, iterate.

               if (m > l) {
                  var iter : Int = 0;
                  do {
                     iter = iter + 1;  // (Could check iteration count here.)

                     // Compute implicit shift

                     var g = d(l);
                     var p = (d(l+1) - g) / (2.0 * e(l));
                     var r = scala.math.sqrt(p*p + 1.0);
                     if (p < 0) {
                        r = -r;
                     }
                     d(l) = e(l) / (p + r);
                     d(l+1) = e(l) * (p + r);
                     var dl1 = d(l+1);
                     var h = g - d(l);
                     //for (int i = l+2; i < n; i++) {
                     for(i <- (l+2) until n){
                        d(i) -= h;
                     }
                     f = f + h;

                     // Implicit QL transformation.

                     p = d(m);
                     var c = 1.0;
                     var c2 = c;
                     var c3 = c;
                     var el1 = e(l+1);
                     var s = 0.0;
                     var s2 = 0.0;

                     //for (int i = m-1; i >= l; i--) {
                     var i = m-1
                     while(i >= l){

                        c3 = c2;
                        c2 = c;
                        s2 = s;
                        g = c * e(i);
                        h = c * p;
                        r = scala.math.sqrt(p*p + e(i)*e(i));
                        e(i+1) = s * r;
                        s = e(i) / r;
                        c = p / r;
                        p = c * d(i) - s * g;
                        d(i+1) = h + s * (c * g + s * d(i));

                        // Accumulate transformation.

                        //for (int k = 0; k < n; k++) {
                        for(k <- 0 until n){
                           h = V(k, i+1);
                           V(k, i+1) = s * V(k, i) + c * h;
                           V(k, i) = c * V(k, i) - s * h;
                        }

                       i = i-1
                     }
                     p = -s * s2 * c3 * el1 * e(l) / dl1;
                     e(l) = s * p;
                     d(l) = c * p;

                     // Check for convergence.

                  } while (scala.math.abs(e(l)) > eps*tst1);
               }
               d(l) = d(l) + f;
               e(l) = 0.0;
            }

            // Sort eigenvalues and corresponding vectors.

            //for (int i = 0; i < n-1; i++) {
            for(i <- 0 until (n-1)){
               var k : Int = i;
               var p = d(i);

               //for (int j = i+1; j < n; j++) {
               for(j <- (i+1) until n){
                  if (d(j) < p) {
                     k = j;
                     p = d(j);
                  }
               }
               if (k != i) {
                  d(k) = d(i);
                  d(i) = p;

                  //for (int j = 0; j < n; j++) {
                  for(j <- 0 until n){
                     p = V(j, i);
                     V(j, i) = V(j, k);
                     V(j, k) = p;
                  }
               }
            }

      }
      else {

            var ort = Vector.zeros(n)
            var H = origMat.clone
            //var ort = new DoubleVectorImpl(true, n)
            //var H = new DoubleMatrixImpl(n,n)
            //for(i <- 0 until n)
              //for(j <- 0 until n)
                //H(i,j) = origMat(i,j)
           // Reduce to Hessenberg form.

            // Nonsymmetric reduction to Hessenberg form.
            //  This is derived from the Algol procedures orthes and ortran,
            //  by Martin and Wilkinson, Handbook for Auto. Comp.,
            //  Vol.ii-Linear Algebra, and the corresponding
            //  Fortran subroutines in EISPACK.

            var low : Int = 0;
            var high : Int = n-1;

            //for (int m = low+1; m <= high-1; m++) {
            for(m <- (low+1) until high){

               // Scale column.

               var scale = 0.0;
               //for (int i = m; i <= high; i++) {
               for(i <- m until (high+1)){
                  scale = scale + scala.math.abs(H(i, m-1));
               }
               if (scale != 0.0) {

                  // Compute Householder transformation.

                  var h = 0.0;

                  //for (int i = high; i >= m; i--) {
                  var i = high
                  while(i >= m){

                     ort(i) = H(i, m-1)/scale;
                     h += ort(i) * ort(i);

                    i = i-1
                  }
                  var g = scala.math.sqrt(h);
                  if (ort(m) > 0) {
                     g = -g;
                  }
                  h = h - ort(m) * g;
                  ort(m) = ort(m) - g;

                  // Apply Householder similarity transformation
                  // H = (I-u*u'/h)*H*(I-u*u')/h)

                  //for (int j = m; j < n; j++) {
                  for(j <- m until n){
                     var f = 0.0;

                     //for (int i = high; i >= m; i--) {
                     var i = high
                     while(i >= m){

                        f += ort(i)*H(i, j);

                       i = i -1
                     }
                     f = f/h;

                     //for (int i = m; i <= high; i++) {
                     for(i <- m until (high+1)){
                        H(i, j) -= f*ort(i);
                     }
                 }

                 //for (int i = 0; i <= high; i++) {
                 for(i <- 0 until (high+1)){
                     var f = 0.0;

                     //for (int j = high; j >= m; j--) {
                     var j = high
                     while(j >= m){

                        f += ort(j)*H(i, j);

                       j = j-1
                     }
                     f = f/h;

                     //for (int j = m; j <= high; j++) {
                     for(j <- m until (high+1)){
                        H(i, j) -= f*ort(j);
                     }
                  }
                  ort(m) = scale*ort(m);
                  H(m, m-1) = scale*g;
               }
            }

            // Accumulate transformations (Algol's ortran).

            //for (int i = 0; i < n; i++) {
               //for (int j = 0; j < n; j++) {
            for(i <- 0 until n){
              for(j <- 0 until n){
                  V(i, j) = if(i == j) 1.0 else 0.0 //(i == j ? 1.0 : 0.0);
               }
            }

            //for (int m = high-1; m >= low+1; m--) {
            var m = high-1
            while(m >= (low+1)){

               if (H(m, m-1) != 0.0) {

                  //for (int i = m+1; i <= high; i++) {
                  for(i <- (m+1) until (high+1)){
                     ort(i) = H(i, m-1);
                  }

                  //for (int j = m; j <= high; j++) {
                  for(j <- m until (high+1)){
                     var g = 0.0;

                     //for (int i = m; i <= high; i++) {
                     for(i <- m until (high+1)){
                        g += ort(i) * V(i, j);
                     }
                     // Double division avoids possible underflow
                     g = (g / ort(m)) / H(m, m-1);

                     //for (int i = m; i <= high; i++) {
                     for(i <- m until (high+1)){
                        V(i, j) += g * ort(i);
                     }
                  }
               }

              m = m-1
            }

           // Reduce Hessenberg to real Schur form.
            // Nonsymmetric reduction from Hessenberg to real Schur form.
            //  This is derived from the Algol procedure hqr2,
            //  by Martin and Wilkinson, Handbook for Auto. Comp.,
            //  Vol.ii-Linear Algebra, and the corresponding
            //  Fortran subroutine in EISPACK.

            // Initialize

            var nn = n;
            var tn = nn-1;
            low = 0;
            high = nn-1;
            var eps = scala.math.pow(2.0,-52.0);
            var exshift = 0.0;
            //double p=0,q=0,r=0,s=0,z=0,t,w,x,y;
            var p = 0.0
            var q = 0.0
            var r = 0.0
            var s = 0.0
            var z = 0.0
            var t = 0.0
            var w = 0.0
            var x = 0.0
            var y = 0.0


            // Store roots isolated by balanc and compute matrix norm

            var norm = 0.0;

            //for (int i = 0; i < nn; i++) {
            for(i <- 0 until nn){
               if (i < low | i > high) {
                  d(i) = H(i, i);
                  e(i) = 0.0;
               }
               //for (int j = scala.math.max(i-1,0); j < nn; j++) {
               for(j <- scala.math.max(i-1,0) until nn){
                  norm = norm + scala.math.abs(H(i, j));
               }
            }

            // Outer loop over eigenvalue index

            var iter : Int = 0;
            while (tn >= low) {

               // Look for single small sub-diagonal element
               var notfound = true
               var l : Int = tn;
               while (l > low && notfound) {
                  s = scala.math.abs(H(l-1, l-1)) + scala.math.abs(H(l, l));
                  if (s == 0.0) {
                     s = norm;
                  }
                  if (scala.math.abs(H(l, l-1)) < eps * s) {
                     //break;
                    notfound = false
                    l = l+1 //cancel out the effects of next line, as to replicate break statement
                  }
                  l = l-1;
               }

               // Check for convergence
               // One root found

               if (l == tn) {
                  H(tn, tn) = H(tn, tn) + exshift;
                  d(tn) = H(tn, tn);
                  e(tn) = 0.0;
                  tn=tn-1
                  iter = 0;

               // Two roots found

               }
               else if (l == tn-1) {
                  w = H(tn, tn-1) * H(tn-1, tn);
                  p = (H(tn-1, tn-1) - H(tn, tn)) / 2.0;
                  q = p * p + w;
                  z = scala.math.sqrt(scala.math.abs(q));
                  H(tn, tn) = H(tn, tn) + exshift;
                  H(tn-1, tn-1) = H(tn-1, tn-1) + exshift;
                  x = H(tn, tn);

                  // Real pair

                  if (q >= 0) {
                     if (p >= 0) {
                        z = p + z;
                     }
                     else {
                        z = p - z;
                     }
                     d(tn-1) = x + z;
                     d(tn) = d(tn-1);
                     if (z != 0.0) {
                        d(tn) = x - w / z;
                     }
                     e(tn-1) = 0.0;
                     e(tn) = 0.0;
                     x = H(tn, tn-1);
                     s = scala.math.abs(x) + scala.math.abs(z);
                     p = x / s;
                     q = z / s;
                     r = scala.math.sqrt(p * p+q * q);
                     p = p / r;
                     q = q / r;

                     // Row modification

                     //for (int j = n-1; j < nn; j++) {
                     for(j <- (tn-1) until nn){
                        z = H(tn-1, j);
                        H(tn-1, j) = q * z + p * H(tn, j);
                        H(tn, j) = q * H(tn, j) - p * z;
                     }

                     // Column modification

                     //for (int i = 0; i <= n; i++) {
                     for(i <- 0 until (tn+1)){
                        z = H(i, tn-1);
                        H(i, tn-1) = q * z + p * H(i, tn);
                        H(i, tn) = q * H(i, tn) - p * z;
                     }

                     // Accumulate transformations

                     //for (int i = low; i <= high; i++) {
                     for(i <- low until (high+1)){
                        z = V(i, tn-1);
                        V(i, tn-1) = q * z + p * V(i, tn);
                        V(i, tn) = q * V(i, tn) - p * z;
                     }

                  // Complex pair

                  }
                  else {
                     d(tn-1) = x + p;
                     d(tn) = x + p;
                     e(tn-1) = z;
                     e(tn) = -z;
                  }
                  tn = tn - 2;
                  iter = 0;

               // No convergence yet

               }
               else {

                  // Form shift

                  x = H(tn, tn);
                  y = 0.0;
                  w = 0.0;
                  if (l < tn) {
                     y = H(tn-1, tn-1);
                     w = H(tn, tn-1) * H(tn-1, tn);
                  }

                  // Wilkinson's original ad hoc shift

                  if (iter == 10) {
                     exshift += x;

                     //for (int i = low; i <= n; i++) {
                     for(i <- low until (tn+1)){
                        H(i, i) -= x;
                     }
                     s = scala.math.abs(H(tn, tn-1)) + scala.math.abs(H(tn-1, tn-2));
                     x =  0.75 * s;
                     y =  0.75 * s;
                     w = -0.4375 * s * s;
                  }

                  // MATLAB's new ad hoc shift

                  if (iter == 30) {
                      s = (y - x) / 2.0;
                      s = s * s + w;
                      if (s > 0) {
                          s = scala.math.sqrt(s);
                          if (y < x) {
                             s = -s;
                          }
                          s = x - w / ((y - x) / 2.0 + s);

                          //for (int i = low; i <= n; i++) {
                          for(i <- low until (tn+1)){
                             H(i, i) -= s;
                          }
                          exshift += s;
                          x = 0.964;
                          y = 0.964
                          w = 0.964
                      }
                  }

                  iter = iter + 1;   // (Could check iteration count here.)

                  // Look for two consecutive small sub-diagonal elements
                  var notfound = true
                  var m : Int = tn-2;
                  while (m >= l && notfound) {
                     z = H(m, m);
                     r = x - z;
                     s = y - z;
                     p = (r * s - w) / H(m+1, m) + H(m, m+1);
                     q = H(m+1, m+1) - z - r - s;
                     r = H(m+2, m+1);
                     s = scala.math.abs(p) + scala.math.abs(q) + scala.math.abs(r);
                     p = p / s;
                     q = q / s;
                     r = r / s;
                     if (m == l) {
                        //break;
                        notfound = false
                        m = m + 1 //to duplicate break's functionality
                     }
                     if( (scala.math.abs(H(m, m-1)) * (scala.math.abs(q) + scala.math.abs(r)) <
                        eps * (scala.math.abs(p) * (scala.math.abs(H(m-1, m-1)) + scala.math.abs(z) +
                        scala.math.abs(H(m+1, m+1))))) && notfound) {
                           //break;
                          notfound = false
                          m = m + 1 //to duplicate break's functionality
                     }
                     m=m-1;
                  }

                  //for (int i = m+2; i <= n; i++) {
                  for(i <- (m+2) until (tn+1)){
                     H(i, i-2) = 0.0;
                     if (i > m+2) {
                        H(i, i-3) = 0.0;
                     }
                  }

                  // Double QR step involving rows l:n and columns m:n

                  //for (int k = m; k <= n-1; k++) {
                  var k = m
                  var notzero = true
                  while(k <= (tn-1) && notzero){
                     var notlast = (k != tn-1);
                     if (k != m) {
                        p = H(k, k-1);
                        q = H(k+1, k-1);
                        //r = (notlast ? H(k+2, k-1) : 0.0);
                        r = if(notlast) H(k+2, k-1) else 0.0
                        x = scala.math.abs(p) + scala.math.abs(q) + scala.math.abs(r);
                        if (x != 0.0) {
                           p = p / x;
                           q = q / x;
                           r = r / x;
                        }
                     }
                     if (x == 0.0) {
                        //break;
                        notzero = false
                     }
                     if(notzero){  //to duplicate break's functionality
                       s = scala.math.sqrt(p * p + q * q + r * r);
                       if (p < 0) {
                          s = -s;
                       }
                       if (s != 0) {
                          if (k != m) {
                             H(k, k-1) = -s * x;
                          } else if (l != m) {
                             H(k, k-1) = -H(k, k-1);
                          }
                          p = p + s;
                          x = p / s;
                          y = q / s;
                          z = r / s;
                          q = q / p;
                          r = r / p;

                          // Row modification

                          //for (int j = k; j < nn; j++) {
                          for(j <- k until nn){
                             p = H(k, j) + q * H(k+1, j);
                             if (notlast) {
                                p = p + r * H(k+2, j);
                                H(k+2, j) = H(k+2, j) - p * z;
                             }
                             H(k, j) = H(k, j) - p * x;
                             H(k+1, j) = H(k+1, j) - p * y;
                          }

                          // Column modification

                          //for (int i = 0; i <= scala.math.min(n,k+3); i++) {
                          for(i <- 0 until (scala.math.min(tn, k+3) + 1)){
                             p = x * H(i, k) + y * H(i, k+1);
                             if (notlast) {
                                p = p + z * H(i, k+2);
                                H(i, k+2) = H(i, k+2) - p * r;
                             }
                             H(i, k) = H(i, k) - p;
                             H(i, k+1) = H(i, k+1) - p * q;
                          }

                          // Accumulate transformations

                          //for (int i = low; i <= high; i++) {
                          for(i <- low until (high+1)){
                             p = x * V(i, k) + y * V(i, k+1);
                             if (notlast) {
                                p = p + z * V(i, k+2);
                                V(i, k+2) = V(i, k+2) - p * r;
                             }
                             V(i, k) = V(i, k) - p;
                             V(i, k+1) = V(i, k+1) - p * q;
                          }
                       } // (s != 0)

                       k = k +1
                     }
                  }  // k loop
               }  // check convergence
            }  // while (n >= low)

            // Backsubstitute to find vectors of upper triangular form

            if (norm != 0.0) {


              //for (n = nn-1; n >= 0; n--) {
              tn = nn-1
              while(tn >= 0){

                 p = d(tn);
                 q = e(tn);

                 // Real vector

                 if (q == 0) {
                    var l : Int = tn;
                    H(tn, tn) = 1.0;

                    //for (int i = n-1; i >= 0; i--) {
                    var i = tn-1
                    while(i >= 0){

                       w = H(i, i) - p;
                       r = 0.0;

                       //for (int j = l; j <= n; j++) {
                       for(j <- l until (tn+1)){
                          r = r + H(i, j) * H(j, tn);
                       }
                       if (e(i) < 0.0) {
                          z = w;
                          s = r;
                       } else {
                          l = i;
                          if (e(i) == 0.0) {
                             if (w != 0.0) {
                                H(i, tn) = -r / w;
                             } else {
                                H(i, tn) = -r / (eps * norm);
                             }

                          // Solve real equations

                          } else {
                             x = H(i, i+1);
                             y = H(i+1, i);
                             q = (d(i) - p) * (d(i) - p) + e(i) * e(i);
                             t = (x * s - z * r) / q;
                             H(i, tn) = t;
                             if (scala.math.abs(x) > scala.math.abs(z)) {
                                H(i+1, tn) = (-r - w * t) / x;
                             } else {
                                H(i+1, tn) = (-s - y * t) / z;
                             }
                          }

                          // Overflow control

                          t = scala.math.abs(H(i, tn));
                          if ((eps * t) * t > 1) {

                             //for (int j = i; j <= n; j++) {
                             for(j <- i until (tn+1)){
                                H(j, tn) = H(j, tn) / t;
                             }
                          }
                       }
                      i = i-1
                    }

                 // Complex vector

                 } else if (q < 0) {
                    var l : Int = tn-1;

                    // Last vector component imaginary so matrix is triangular

                    if (scala.math.abs(H(tn, tn-1)) > scala.math.abs(H(tn-1, tn))) {
                       H(tn-1, tn-1) = q / H(tn, tn-1);
                       H(tn-1, tn) = -(H(tn, tn) - p) / H(tn, tn-1);
                    } else {
                       val cdivs = cdiv(0.0,-H(tn-1, tn),H(tn-1, tn-1)-p,q);
                       H(tn-1, tn-1) = cdivs._1;
                       H(tn-1, tn) = cdivs._2;
                    }
                    H(tn, tn-1) = 0.0;
                    H(tn, tn) = 1.0;

                    //for (int i = n-2; i >= 0; i--) {
                    var i = tn-2
                    while(i >= 0){

                       //double ra,sa,vr,vi;
                       var ra = 0.0
                       var sa = 0.0
                       var vr = 0.0
                       var vi = 0.0

                       //for (int j = l; j <= n; j++) {
                       for(j <- l until (tn+1)){
                          ra = ra + H(i, j) * H(j, tn-1);
                          sa = sa + H(i, j) * H(j, tn);
                       }
                       w = H(i, i) - p;

                       if (e(i) < 0.0) {
                          z = w;
                          r = ra;
                          s = sa;
                       } else {
                          l = i;
                          if (e(i) == 0) {
                             val cdivs = cdiv(-ra,-sa,w,q);
                             H(i, tn-1) = cdivs._1;
                             H(i, tn) = cdivs._2;
                          } else {

                             // Solve complex equations

                             x = H(i, i+1);
                             y = H(i+1, i);
                             vr = (d(i) - p) * (d(i) - p) + e(i) * e(i) - q * q;
                             vi = (d(i) - p) * 2.0 * q;
                             if (vr == 0.0 & vi == 0.0) {
                                vr = eps * norm * (scala.math.abs(w) + scala.math.abs(q) +
                                scala.math.abs(x) + scala.math.abs(y) + scala.math.abs(z));
                             }
                             val cdivs = cdiv(x*r-z*ra+q*sa,x*s-z*sa-q*ra,vr,vi);
                             H(i, tn-1) = cdivs._1;
                             H(i, tn) = cdivs._2;
                             if (scala.math.abs(x) > (scala.math.abs(z) + scala.math.abs(q))) {
                                H(i+1, tn-1) = (-ra - w * H(i, tn-1) + q * H(i, tn)) / x;
                                H(i+1, tn) = (-sa - w * H(i, tn) - q * H(i, tn-1)) / x;
                             } else {
                                val cdivs = cdiv(-r-y*H(i, tn-1),-s-y*H(i, tn),z,q);
                                H(i+1, tn-1) = cdivs._1;
                                H(i+1, tn) = cdivs._2;
                             }
                          }

                          // Overflow control

                          t = scala.math.max(scala.math.abs(H(i, tn-1)),scala.math.abs(H(i, tn)));
                          if ((eps * t) * t > 1) {

                             //for (int j = i; j <= n; j++) {
                             for(j <- i until (tn+1)){
                                H(j, tn-1) = H(j, tn-1) / t;
                                H(j, tn) = H(j, tn) / t;
                             }
                          }
                       }

                      i = i-1
                    }
                 }

                tn = tn-1
              }

              // Vectors of isolated roots

              //for (int i = 0; i < nn; i++) {
              for(i <- 0 until nn){
                 if (i < low | i > high) {

                    //for (int j = i; j < nn; j++) {
                    for(j <- i until nn){
                       V(i, j) = H(i, j);
                    }
                 }
              }

              // Back transformation to get eigenvectors of original matrix

              //for (int j = nn-1; j >= low; j--) {
              var j = nn-1
              while(j >= low){

                 //for (int i = low; i <= high; i++) {
                 for(i <- low until (high+1)){
                    z = 0.0;

                    //for (int k = low; k <= scala.math.min(j,high); k++) {
                    for(k <- low until (scala.math.min(j,high) +1 )){
                       z = z + V(i, k) * H(k, j);
                    }
                    V(i, j) = z;
                 }

                j = j-1
              }

            }
      } //END FINDING EIGENVALUES


      //make the eigenvalues matrix, then return eigenvectors and eigenvalues
      var EVs = Matrix.zeros(n,n)
      for(i <- 0 until d.length){
        EVs(i,i) = d(i)
      }

      var toReturn = Vector[Matrix[Double]](2)
      toReturn(0) = V
      toReturn(1) = EVs
      toReturn
    }
  }

    //Helper method for complex scalar division.
  protected def cdiv(xr : Double, xi : Double, yr : Double, yi : Double) : Tuple2[Double, Double] = {
    //double r,d;
    var cdivr = 0.0
    var cdivi = 0.0
    var r = 0.0
    var d = 0.0

    if (scala.math.abs(yr) > scala.math.abs(yi)) {
      r = yi/yr;
      d = yr + r*yi;
      cdivr = (xr + r*xi)/d;
      cdivi = (xi - r*xr)/d;
    } else {
      r = yr/yi;
      d = yi + r*yr;
      cdivr = (r*xr + xi)/d;
      cdivi = (r*xi - xr)/d;
    }

    val toReturn = (cdivr, cdivi)
    return toReturn
  }
}
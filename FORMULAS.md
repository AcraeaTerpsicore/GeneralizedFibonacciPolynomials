# Core GFP Formulas
- **Fibonacci-type recurrence**: $\mathcal{F}_0(x)=0$, $\mathcal{F}_1(x)=1$, and for $n \ge 2$,
  $\mathcal{F}_n(x)=d(x)\,\mathcal{F}_{n-1}(x)+g(x)\,\mathcal{F}_{n-2}(x)$.
- **Lucas-type recurrence**: $\mathcal{L}_0(x)=p_0$, $\mathcal{L}_1(x)=p_1(x)$, and for $n \ge 2$,
  $\mathcal{L}_n(x)=d(x)\,\mathcal{L}_{n-1}(x)+g(x)\,\mathcal{L}_{n-2}(x)$
  with $p_0 \in \{\pm 1,\pm 2\}$ and $d(x)=\alpha\,p_1(x)$ where $\alpha=2/p_0$.
- **Binet representations** (for $d(x)^2+4 g(x)\neq 0$):
  $$
  \mathcal{F}_n(x)=\frac{a(x)^n-b(x)^n}{a(x)-b(x)}, \qquad
  \mathcal{L}_n(x)=\frac{a(x)^n+b(x)^n}{\alpha},
  $$
  where $a(x)=\dfrac{d(x)+\sqrt{d(x)^2+4 g(x)}}{2}$ and $b(x)=\dfrac{d(x)-\sqrt{d(x)^2+4 g(x)}}{2}$.
- **Algebraic relations between characteristic roots**:
  $a(x)+b(x)=d(x)$, $a(x)\,b(x)=-g(x)$, and $a(x)-b(x)=\sqrt{d(x)^2+4 g(x)}$.
- **Generalized Hoggatt expansions** (Lemmas 2.6–2.7):
  $$
  \mathcal{F}_n(x)=\sum_{i=0}^{\lfloor (n-1)/2\rfloor} \binom{n-i-1}{i}\,d(x)^{\,n-2i-1}\,g(x)^{\,i}, \qquad n\ge 1,
  $$
  $$
  \mathcal{L}_n(x)=\frac{1}{\alpha} \sum_{i=0}^{\lfloor n/2\rfloor} \frac{n}{n-i}\binom{n-i}{i}\,d(x)^{\,n-2i}\,g(x)^{\,i}, \qquad n\ge 1,
  $$
  with $\mathcal{F}_0(x)=0$ and $\mathcal{L}_0(x)=p_0$.
- **Parity lemma** (Lemma 2.3 of the paper): if $d(x)$ is odd and $g(x)$ is even, then $\mathcal{F}_n(-x)=(-1)^{n+1}\mathcal{F}_n(x)$ and likewise $\mathcal{L}_n(-x)=(-1)^{n+1}\mathcal{L}_n(x)$.
- **Chebyshev-type orthogonality weight** (Proposition 3.4): when $g(x)$ is a nonzero constant and $h(x)=d(x)/\sqrt{-4 g(x)}$ maps the integration interval into $[-1,1]$, 
  $$
  \int \mathcal{F}_n(x)\,\mathcal{F}_m(x)\,\sqrt{-4 g(x)-d(x)^2}\,d'(x)\,dx = 0 \quad (n\neq m),
  $$
  and
  $$
  \int \frac{\mathcal{L}_n(x)\,\mathcal{L}_m(x)}{\sqrt{-4 g(x)-d(x)^2}}\;d'(x)\,dx = 0 \quad (n\neq m).
  $$
- **Affine $d(x)$ orthogonality corollary** (Corollary 3.5): for $d(x)=c x^t+h$ with odd $t>0$ and $g(x)=-k/4$ ($k>0$),
  $$
  \omega(x)=\sqrt{k-d(x)^2}\;x^{t-1},
  $$
  $$\int_{-s_1}^{s_2} \mathcal{F}_n(x)\,\mathcal{F}_m(x)\,\omega(x)\,dx = 0 \quad (n\neq m),$$
  $$\int_{-s_1}^{s_2} \frac{\mathcal{L}_n(x)\,\mathcal{L}_m(x)}{\omega(x)}\,dx = 0 \quad (n\neq m),$$
  where $s_1=\left(\dfrac{\sqrt{k}+h}{c}\right)^{1/t}$ and $s_2=\left(\dfrac{\sqrt{k}-h}{c}\right)^{1/t}$.
- **Random-walk coefficients** (Proposition 4.1): for $d(x)=c x+h$ and $g(x)=-(c-1+h)$ with $h\le 0$ and $c > 1-h>0$,
  the discrete-time birth-and-death chain has 
  $p_n=\dfrac{1}{c}$, $q_n=\dfrac{c-1+h}{c}$, and $r_n=\dfrac{-h}{c}$;
  the continuous-time version with $g(x)=-k/4$ ($k>0$, $c<0$) yields rates
  $\lambda_n=\dfrac{-1}{c}$, $\mu_n=\dfrac{-k}{4 c}$, and diagonal entry $\beta_n=\dfrac{4+k}{4 c}$.
- **Quadrature diagnostics**: numerical orthogonality checks integrate $\mathcal{F}_n(x)\mathcal{F}_m(x)\,\omega(x)$ or $\mathcal{L}_n(x)\mathcal{L}_m(x)/\omega(x)$ over the intervals prescribed above, combining symmetry predictions from Lemma 2.3 with these weight functions.
- **Potential coefficients** (Section 4): setting $\pi_0=1$ and 
  $\pi_n=\dfrac{p_0 p_1 \cdots p_{n-1}}{q_1 q_2 \cdots q_n}$ for discrete time (or
  $\pi_n=\dfrac{\lambda_0 \lambda_1 \cdots \lambda_{n-1}}{\mu_1 \mu_2 \cdots \mu_n}$ in continuous time) characterises recurrence/ergodicity, converging when $|p_n/q_n|<1$ (resp. $|\lambda_n/\mu_n|<1$).

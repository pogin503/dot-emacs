=begin

= howm (��ͤ���� Wiki ��ɤ�)
$Id: README.ja.rd,v 1.309.2.7 2011-12-28 09:21:41 hira Exp $

Emacs ������Ū�ʥ���ɤ�ɤ�Ȥ뤿��δĶ��Ǥ�.
ʬ�ൡǽ�Ϥ����ƤĤ��ޤ���.
������, ��ʸ�����ȥ��֥�󥯤���ڤˤǤ���褦�ˤ��ޤ���.
��ͳ�񼰤ʤΤǲ��������� :-)

== �ܼ�

* ((<�Ȥ���>)) ��
  ((<����񤳤�>))��((<�����ɤ⤦>))��((<��ޥ����>))
* ((<Ƴ��ˡ>)) ��
  ((<���󥹥ȡ���>))��((<�������ޥ���>))��((<�����ġ���>))
* ((<����>)) ��
  ((<�����ˤĤ���>))��((<ư���ޤ����?>))
* ((<����>)) ��
  ((<����>))��((<������Ͽ>))��((<���ɥ쥹>))

== �Ȥ���

* �������ꤹ���Ʋ��������
  �� ���((<���塼�ȥꥢ��|URL:TUTORIAL.ja.rd>))��ɤ���
* ��ͳ�٤��ꤹ���Ƥɤ��Ȥ��Ф������
  �� ((<����ʤդ��˻Ȥ��ޤ�|URL:index-j.html#hint>))

=== ����񤳤�
(howm-mode)

* ���
  * C-c , , (M-x howm-menu) �ǥ�˥塼��Ф�,
    [����] �˥��������֤��ƥ꥿���� �� �ֺ����Υ��פ򳫤�
    * �ޤ���, ľ�� C-c , c (M-x howm-create)
  * ����ʥƥ�ץ졼�Ȥ�ɽ�������
      = �� ��(�����ȥ���)
      [2002-09-16 20:08] >>> /home/hira/elisp/howm/howm-mode.el
      ��(������ & �������˸��Ƥ��ե�����)
    * ���Ƥ��ե����뤬���פʤ�, undo (C-x u ���� C-_ ����) �Ǿä�
    * �ƥ�ץ졼�ȼ��Τ����פʤ�, ³���Ƥ⤦���� undo
  * �����ʤ��Ȥ򹥤��ʤ褦�˽�
  * �������.

* �����ȥ��� (1 �ե�����ʣ�����)
  * ���Τ褦�˽񤯤�, foo �� bar �������ȥ�
      = foo
      (�� �ۤ��ۤ� ��)
      = bar
      (�� �դ��դ� ��)
    * Ϣ��ɽ���Ǥ�, (�� �ۤ��ۤ� ��) �� (�� �դ��դ� ��) ���ҤȤ����ޤ�
  * ���Τʽ񼰤�,
      (��Ƭ)=(����)(�����ȥ�)(����)
    * �񼰤��ѹ��� (��((<�������ޥ���>)))
  * �����ȥ�ʤ���, ñ�˥��ζ��ڤ�Ȥ��ƻȤ������Ǥ� OK
      (�� �ۤ��ۤ� ��)
      = 
      (�� �դ��դ� ��)
      = 
      (�� �ؤ�ؤ� ��)

* ���Τ褦�˽񤯤ȥ��
  * goto ���: �ե�����(�ǥ��쥯�ȥ�)̾ or �ޤޤ��ʸ����
      >>> ~/.emacs
      >>> /usr/src
      >>> �ۤ��ۤ�
    * ��ʸ��ˡ֤ۤ��ۤ��פȤ���ʸ�����ޤ���ؤΥ��
  * come-from ���: ¾�Υ��ǡ֤դ��դ��פȤ���ʸ���󤬽ФƤ�����,
    ����֤��Υ��ؤΥ�󥯤�
      <<< �դ��դ�
    * ����
      ((<"Jargon: COME-FROM"|URL:http://catb.org/~esr/jargon/html/C/COME-FROM.html>))
  * Wiki �����: goto ��Ʊ��. ��������<<< �ؤ�ؤ�פ��⤷�ʤ���к��.
      [[�ؤ�ؤ�]]

* ��󥯤ˤϲ������������. �����˥���������äƤäƥ꥿���󥭡�!
  * �����ե�����ΰ�����ɽ������� (��((<�����ɤ⤦>)))
    * ���Ȥ���, ��>>> emacs�פʤ餳��ʰ���
        <<< emacs             �� ���Ф�� come-from ����򤷤����
        <<< emacs lisp        �� ��emacs�פ�ޤ� come-from ������������
        <<< ���� emacs lisp
        grep, ruby, emacs �� regexp �ΰ㤤 �� ��ʸ��ˡ�emacs�פ�ޤ���
        emacs �Ѹ����ġ���?[2001-08-13]       (��������)
        ��
    * �ɤߤ������˥������뤢�碌�ƥ꥿���󥭡�!
      �� ���Υ��򳫤�
  * ΢��
    * come-from ��󥯤� <<< ��ǥ꥿���� �� �ִ�Ϣ������ɡפؤΥ��
      * ��
        * �ּ���ס�lisp�פ� come-from ������ɤΤȤ�
        * ��<<< ����� lisp�פξ�ǥ꥿����
          �� �ּ���ס�lisp�פ�ޤ७����ɤ��ҥå�
    * �����ˡ�<<< foo <<< bar <<< baz�פȽ񤱤�, ��alias��
      * foo, bar, baz �Τɤ�ǥ꥿�����á���Ƥ�
        ��foo �ޤ��� bar �ޤ��� baz�פθ����ˤʤ�ޤ�
      * Tips: �ߤ��ˤ����ꤿ������ alias �Ǻ�����ΤϷ�, �Ȥ������ˤϡ�
        * �ɤ����ˡ�<<< foo��
        * �̤Τɤ����ˡ�foo <<< bar��
        * ���������, foo��bar �ɤ���Ǹ������Ƥ��foo <<< bar�פ���̤�

* ��󥯤ο���
  * �¤�ñ��, ��grep �դ��դ��פΥ��硼�ȥ��åȤ��ä���
  * come-from ��󥯤θ���
    * ���Υ�����ɤ��ФƤ�����, ��ưŪ�˥��(= ����)�ˤ��Ƥ���
    * ���Υ�����ɤ򸡺������Ȥ���, ��Ƭ��ɽ�����Ƥ���
  * come-from ��󥯤ϡ�
    * �ʤ��Ƥ�褷
    * 1 �ĤΥ����� 2 �� 3 �ĤȤ��äƤ�褷
    * �̤Υ���Ʊ��������ɤ����֤äƤ�褷
    * �����ȥ�ȷ�ͤ�ʤ�,
        = <<< �դ��դ�
  * come-from, goto �Ȥ�, ��ʸ����ʸ������� (��((<�������ޥ���>)))
  * �񼰤��ѹ��� (��((<�������ޥ���>)))
  * �ʲ�, come-from ��󥯤Υ�����ɤ�ñ�ˡ֥�����ɡפ�ɽ��

* action-lock
  * ��ʸ�ξ�ǥ꥿���󥭡�����������ˡȯư
  * { } �Ƚ񤯤ȡ֥ȥ��륹���å���.
    ����������Ӥ� { } �� {*} �� {-} �� { } �� ��
  * {_} �Ƚ񤯤ȡ�̤������.
    �������� {_} �� [2002-09-13 02:31]
  * http://�� �� �֥饦����ư
    * browse-url �����. ɬ�פʤ�Ŭ��������.
        (setq browse-url-browser-function 'browse-url-mozilla)
  * file://�� �� �ե�����򳫤�
    * C-u RET �ʤ����ʬ�䤷�Ƴ���
  * [2002-10-18] �Τ褦�����շ����ξ�ǥ꥿���� �� minibuffer �ǡ�
    * ���Τޤޥ꥿���� �� �������դ򸡺� (goto link)
    * ��+17�� �� 17 ��������դ˽񤭤���
    * ��20030921�� �� [2003-09-21] �˽񤭤���
      * ǯ���Ͼ�ά��ǽ
        * ��6�� �� [2002-10-06]
        * ��803�� �� [2002-08-03]
        * ��31103�� �� [2003-11-03]
    * ��~20031030�� �� ���ιԤ�ʣ���� [2003-10-30] ʬ�ޤ�����
      * ǯ���Ͼ�ά��ǽ (���Ʊ��)
      * ��Every?�פ��Ф���
        * ���Τޤޥ꥿���� �� ����
        * 3 �� 3������
        * w �� �轵
        * m �� ���
        * y �� ��ǯ
    * ��.�� �� ���������դ˽񤭤���
    * ���ʤߤ�, ��˥塼�� [����] �����շ��������ϤǤ��ޤ�
  * ��󥯤⤳�μ�ʸ�ΰ��
    * ¾�ˤ��⤷�������ǥ����ä��鶵���Ƥ�������

* ���ޥ�� (���� howm-mode �ʳ��Ǥ���ͭ��)
  * C-c , , �� ��˥塼�򳫤� ��
  * ��˥塼
    * ����
      * [space] �� [backspace] �� ��������
      * TAB (M-TAB) �� ��(��)�ι��ܤ�
      * [����] �� > �ξ�ǥ꥿���� �� �¹� (������)
      * ? �� �إ��
      * q �� æ��
    * �ܥ��� [����] (���ޥ��)
      * ����
        * [����] (C-c , c) �� ���������� (���꡼����󤬥����ȥ�) ��
        * [ʣ��] (C-c , D) �� ������ʣ�� (����Ͽ�ƥ�ץ졼�Ȥʤɤ����Ӥ�����)
      * ����
        * [����] (C-c , a) �� �����ΰ��� ��
        * [�Ƕ�] (C-c , l) �� �Ƕ�Υ���Ϣ��ɽ�� ��
          * (C-u 20 C-c , l) �� �Ƕ� 20 ��ʬ�ΰ���
        * [����] (C-c , A) �� ����Υ�� (���Ƥ������濴�����������ս����)
          * �оݥե������(�Խ��⡼�ɤ�)���������֤����˥塼��Ƥ֤���
        * [����] (C-c , h) �� �������� ��
        * [ͽ��] (C-c , y) �� ͽ��ɽ: ((<��ޥ����>))���� ��
        * [Todo] (C-c , t) �� todo ����: ((<��ޥ����>))���� ��
      * ����
        * [����] (C-c , g) �� ����ɽ���θ��� ��
          * ����Ū�ˤ���ʸ����ʸ���ζ��̤ʤ�
            * ��Wiki�פΤ褦������Ū����ʸ������ꤷ���Ȥ��϶���
        * [����] (C-c , s) �� ������ɤ��䴰���Ϥ��Ƹ���ʸ����θ��� ��
          * C-u C-c , g �� C-u C-c , m �Ǥ�
        * [roma] (C-c , m) �� ���޻����� (migemo) ��
        * [����] (C-c , .) �� �����Υ�� ��
          * (C-u 20 C-c , .) �� 20 �����Υ��
        * [����] (C-c , :) �� �����Υ�� ��
          * (C-u 20 C-c , :) �� 20 �����Υ��
      * �Խ�: �оݥե������(�Խ��⡼�ɤ�)���������֤����˥塼��Ƥ֤���
        * [����] (C-c , r) �� ����������ʤ���
        * [����] (C-c , i) �� ������ɤ��䴰���Ϥ���Ž��Ĥ� ��
          * Tips: M-v �Ǹ�������˰ܤä� migemo ��������ȳ�
        * [����] (C-c , d) �� ���������� [yyyy-mm-dd] ��Ž��Ĥ� ��
        * [����] (C-c , T) �� ���������� [yyyy-mm-dd HH:MM] ��Ž��Ĥ� ��
        * [�ꢬ] (C-c , K) �� �����Υ����ȥ�� kill ring �� (C-y ��Ž��Ĥ�) ��
          * �����ȥ뤬�ߤĤ���ʤ��ä��Ȥ��ϥե�����̾
        * [̾��] (C-u C-c , K) �� �ե�����̾�� kill ring �� ��
      * ����
        * [menu ����] (R) �� ��˥塼��ͽ��ɽ�ʤɤ򹹿�
        * [menu �Խ�] �� ��˥塼���Խ�
        * [����] (C-c , Q) �� howm-mode �ʥХåե��򤹤٤ƾä� (̤��¸�Ͻ���) ��
        * [����] (C-c , w) �� ������˥�󥯤򤿤ɤäƼ�ư����. C-g �����. ��
  * ����¾
    * [return] �� ��󥯾�ʤ鳺���ե�����򳫤�. ����ʤ��в���.
    * ��ư
      * C-c , n �� ���Υ�󥯤�
      * C-c , p �� ���Υ�󥯤�
      * ��ե�����ʣ�����ΤȤ���
        * C-c , N �� ���Υ���
        * C-c , P �� ���Υ���
        * C-c , H �� �ǽ�Υ���
        * C-c , L �� �Ǹ�Υ���
    * �������
      * C-c , C �� ���޳����Ƥ�ե�������ɲ�
        * ��˥塼�� [�ɲ�] �Ƚ񤯤�, ����ư��Υܥ���.
          �Ѹ��˥塼�ʤ� [Add].
      * C-c , I �� �ե�����̾���ư�� (��侩)
        * C-u C-c , I �ʤ�, �����ȥǥ��쥯�ȥ��
    * narrow (1 �ե�����ʣ�����ΤȤ�)
      * M-x howm-narrow-to-memo �� ����Υ��򱣤�. �᤹�ˤ� M-x widen
      * M-x howm-toggle-narrow �� �ֱ����סָ�����פ�ȥ���
    * C-c , SPC �� howm �ʥХåե��� howm �Ǥʤ��Хåե��Ȥ��ڤ��ؤ� ��
    * M-x howm-show-buffer-as-howm �� ���Хåե��Υ��ԡ��� howm-mode ��ɽ�� ��
      * ���������ʤΤ��ͻҸ�[2003-09-29]

=== �����ɤ⤦
(�����⡼��)

* ���ޥ��(�Ʒ�)
  * C-c , , (M-x howm-menu) �� ��˥塼
  * C-c , a (M-x howm-list-all) �� ��������
  * C-c , g (M-x howm-list-grep) �� ����⸡�� (����ɽ��)
  * C-c , s (M-x howm-list-grep-fixed) �� ����⸡�� (���ꥭ�����)

* �������󥯥����פ򤹤��, �����⡼��
  * �ǥե���Ȥϰ���ɽ��
    * �����Хåե� + ���ƥХåե�
    * ����������֤Υ������Ƥ�ɽ�������
  * Ϣ��ɽ����Ǥ���
    * @ ��Ϣ��ɽ��. �⤦���� @ �ǰ���ɽ�������.
    * �ҥåȤ����������Ƥ򤼤�֤Ĥʤ���ɽ��
      * ����Ū�ʥ���ɤ�ɤ�� �� �Ĥʤ����ɤ�
    * [tab] �� [alt]-[tab] �Ǽ�/���Υ���
    * Tips: ����õ���Ȥ�, �����Ǥ������٤��ܤꤳ�����,
      Ϣ��ɽ������ migemo ��������ȳ�
  * ����ɽ����
    * 0 �� Ϣ��ɽ���Υȥ��� (@ ��Ʊ��)
    * 1 �� ���ƥХåե���ä�
    * 2 �� ���ƥХåե���Ф�
    * v �� ���ƥХåե���ȥ���
    * TAB, M-TAB �� �������Υե������
    * T �� �����ȥ�ɽ����ȥ���
  * �ɤ����ɽ���Ǥ�
    * n �� p �� �岼
    * [space] �� [backspace] �� ��������
    * j �� k �� ��ԥ�������
    * [return] �� ����������֤Υ��򳫤�
      * C-u ���� [return] �� ���򳫤��ư�����ä�
    * X �� Dired-X ��ư (��̾������ʤɤΥե��������)
      * Dired-X �λȤ�����, info dired-x ���򻲾�
          v �� ��Ȥ򸫤� (q �� ���)
          d �� �־ä����ץޡ���
          x �� �ޡ��������ե����뤿���������˾ä�
    * ? �� �إ��
    * q �� æ��

* ������
  * S �� ���ǥ����Ȥ��뤫ʹ���Ƥ��� (�䴰����)
    * name: �ե�����̾
    * name-match: ���ꤷ���ե�����̾���̤˰ܤ�
    * date: ������
    * mtime: ��������
    * summary: ���ɽ����ʸ����
    * summary-match: ���ꤷ������ɽ������ɽ�����鸡������, ��̤˰ܤ�
    * summary-match-string: Ʊ�� + �ޥå�����ʸ�����˥�����
    * random: �����ॷ��åե�
    * reminder: ��ޥ������
    * numerical-name: �ե�����̾ (������. �᡼��Υ����Ȥ�����)
    * reverse: ��ɽ���εս�
  * C-u S �ʤ�ǥե���Ȥεս�
  * R �� reverse

* �ʤꤳ�� (and ����)
  * f �� ���ǹʤꤳ�फʹ���Ƥ��� (�䴰����)
    * name: �ե�����̾
    * date: ������
    * mtime: ��������
    * summary: ���ɽ����ʸ����
    * contents: ����
    * reminder: ��ޥ�����������ϰ�
    * Region: �ΰ�
    * Around: ����������֤μ���
      * C-u 7 f �� Around �ʤ�, ���� 7 ��
    * uniq: Ʊ���ե�������ǲ��ս�ҥåȤ��Ƥ�, �ǽ�ΰ�ս����ɽ��
  * C-u f �ʤ�, �ޥå�������Τ������
  * G �� contents
  * u �� uniq

* howm-mode �ȶ���
  * l �� �����ΰ���
  * g �� ���� (grep)
    * C-u g �� ������ɤ��䴰���Ϥ��Ƹ���
  * m �� ���޻����� (migemo)
    * C-u m �� C-u g ��Ʊ��
  * c �� �����ե�������� (���꡼����󤬥����ȥ�)
  * Q �� howm-mode �ʥХåե��򤹤٤ƾä� (̤��¸�Ͻ���)

* ����¾
  * ����ɽ���� !  �� shell �ǥ��ޥ�ɼ¹�
    * �����ü���᤯�ΤƤ������, ����� mv �ʤ� rm �ʤꤷ�Ƥ�������
    * 2 ���ܤ���Ͼ���������ư�򤷤ޤ� :-)
  * >>> hoge.png �ʤ鳰�� viewer �ǲ����򳫤�
    * �����((<�������ޥ���>))����

=== ��ޥ����
(ͽ��ɽ��todo)

* ��ǽ
  * ������
      [2002-10-20]+ �ϥ����������� �㤪��
    �Τ褦�˽񤤤Ƥ�����, �����Ǹ��뤳�Ȥ��Ǥ��ޤ�
    * C-c , y �� ͽ��ɽ
      * . �� ������
    * C-c , t �� todo ����
      * ������ξ岼�ɤΰ��֤�ɽ������뤫��, ���դȼ��ष����
  * �ֺǶ��ͽ��פȡ�todo ��Ƭ�פϥ�˥塼�ˤ�ɽ������ޤ�
    (���Ȥ��뤴�Ȥˤ���äȸ�����Τ����פ���)
    * ��˥塼�Ǥ�, ��Ƭ�Ρ�>�׾�� RET ��á���ȥ������Ӥޤ�
      (����ʳ��ΰ��֤Ǥ�, �������ʤ���ʤ�Ʊ��)
  * �����������ե� plan �ؤ� export ���ǽ (��((<�����ġ���>)))

* ��
  * �н� (-)
      [2002-10-20]- �ϥ����������� �㤪��
    * ���������⤭������, �ʸ�Ͻ���������
    * �������ޤǤ��������
    * ����Τ��٤�����ˤ�, ͱͽ�����ǻ���(�ǥե���� 1 ��)
        [2002-10-20]-14 �ϥ����������� �㤪�� �� 14 ���֤��餤�ϵ��ˤ����褦
  * todo (+)
      [2002-10-20]+ �ϥ����������� �㤦�٤�
    * ����������, �������⤭�����äƤ���
    * �������ޤǤ��������
    * �⤭������®����, ͱͽ�����ǻ���(�ǥե���� 7 ��)
        [2002-10-20]+14 �ϥ����������� �㤦�٤� �� 14 ���֤��餤�Τ�����
  * ���� (!)
      [2002-10-20]! �ϥ����������� ����
    * ����������Ť���, �⤭�����äƤ���
    * �������ʹߤ�, ���־���⤭�äѤʤ�
    * �����������⤭�Ϥ���뤫��, ͱͽ�����ǻ���(�ǥե���� 7 ��)
        [2002-10-20]!14 �ϥ����������� ���� �� 14 �������餤����ܤ��ܤ�
    * ͽ��ɽ(���)�ˤ�ɽ��
  * ��α (~)
      [2002-10-20]~ �ϥ����������� �㤪����
    * ����������, �⤭���ߤ򤯤꤫����
    * �������ޤǤ��������
    * �����������⤭���ߤ��뤫��, ͱͽ�����ǻ���(�ǥե���� 30 ��)
        [2002-10-20]!14 �ϥ����������� �㤪���� �� 14 ������
  * ͽ�� (@)
      [2002-10-20]@ �ϥ�����������
    * todo �����ǤϤʤ�, ͽ��ɽ��ɽ��
  * �� (.)
      [2002-10-20]. �ϥ�����������
    * �����

* action-lock
  * ��
      [2002-10-20]+9 �ۤ��ۤ�
    �Ρ�+9�פ˥��������֤��ƥ꥿���󤿤�����, �ߥ˥Хåե��˥�˥塼���Фơ�
    * ���Τޤޥ꥿���� �� �ֺѡ�
        [2002-10-20]. [2002-10-20]:+9 �ۤ��ۤ�
    * x ������ �� ��cancel��
        [2002-10-20]. cancel [2002-10-20]:+9 �ۤ��ۤ�
    * - ������ �� �����н���ѹ�
        [2002-10-20]-9 �ۤ��ۤ�
    * 14 ������ �� ͱͽ������ 14 �����ѹ�
        [2002-10-20]+14 �ۤ��ۤ�
  * ��˥塼��ͽ��ɽ��todo ���������ľ��á���ޤ�

* Tips (��λȤ���)
  * ��todo�פ�֡��ڡפ�������ɬ�פʤ�Τ���
    * ����ʳ��ϡֳн�פ�����ˤޤ����� (�ɤ��������ϤǤ��ޤ��� :p)
    * ���᤿�����, ͱͽ������Ĺ���ֳн�פ�
        [2002-11-10]-10 �ϥ�����������
  * �۵ޤǤϤʤ������פʤ���
      [2002-11-10]-999 ���ϥ�����������
  * ��Ω������������
      [2002-11-10]! �����ϥ�����������

== Ƴ��ˡ

=== ���󥹥ȡ���

==== ��ư���󥹥ȡ���ξ��

* ���󥹥ȡ���
  * ./configure ���� make ����, root �ˤʤä� make install
    * *.el, *.elc �� /usr/share/emacs/site-lisp/howm/ ��
    * doc/, ext/ �� /usr/local/share/howm/ ��
  * xemacs �ξ��
      ./configure --with-xemacs
    * *.el, *.elc �� /usr/lib/xemacs/site-lisp/howm/ ��
  * ���󥹥ȡ�������ѹ���
      ./configure --with-howmdir=$HOME/elisp --prefix=$HOME
    * *.el, *.elc �� ~/elisp/ ��
    * doc/, ext/ �� ~/share/howm/ ��
  * ����¾�Υ��ץ����� ./configure --help �򻲾�
* ����
  * ~/.emacs (.emacs.el ����)���ɲ�
    * case 1: emacs ��ư�����ɤ߹���
        (setq howm-menu-lang 'ja)
        (require 'howm)
    * case 2: �Ϥ���� C-c , , ���������ɤ߹���
        (setq howm-menu-lang 'ja)
        (global-set-key "\C-c,," 'howm-menu)
        (autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki" t)
    * �������, �⤷��Cannot open load file�פȤ����顼���Ф�ʤ�,
      �嵭�����ˤ�����ɲ�
        (add-to-list 'load-path "/usr/share/emacs/site-lisp/howm/")
  * ~/howm/ �κ������˥塼�ե�����Υ��ԡ������פǤ�
    (��˥塼��ư���˼�ư����)

==== ��ư���󥹥ȡ���ξ��

* *.el ��Ŭ���ʾ����֤� (��: ~/elisp/howm)
  * ~/.emacs (.emacs.el ����)��
    * �֤���˱�����, ���Τ褦�˵���
        (add-to-list 'load-path "~/elisp/howm/")
    * �����, ((<��ư���󥹥ȡ���ξ��>))��Ʊ�ͤε��Ҥ��ɲ�
  * �����ߤ�, �Х��ȥ���ѥ���
      cd ~/elisp/howm
      \emacs -batch -q --no-site-file --eval '(progn (add-to-list (quote load-path) ".") (byte-recompile-directory "." 0))'

==== ���󥹥ȡ������­

* �����ߤ�, ~/.emacs ��������ɲ� (��((<�������ޥ���>)))
    ;; ������
    (define-key global-map [katakana] 'howm-menu) ; [��������] �����ǥ�˥塼
    (setq howm-file-name-format "%Y/%m/%Y_%m_%d.howm") ; 1 �� 1 �ե�����
    (setq howm-keyword-case-fold-search t) ; <<< ����ʸ����ʸ������̤��ʤ�
    (setq howm-list-title nil) ; �������˥����ȥ��ɽ�����ʤ�
    (setq howm-menu-refresh-after-save nil) ; save ���˥�˥塼��ư��������
    (setq howm-refresh-after-save nil) ; save ���˲��������ľ���ʤ�
    (setq howm-menu-expiry-hours 2) ; ��˥塼�� 2 ���֥���å���
    (add-to-list 'auto-mode-alist '("\\.howm$" . rd-mode)) ; ���� rd-mode ��

* �ʤ�, ������ɰ����� ~/.howm-keys �˵�Ͽ�����
  * �������Ƥ�, �ƹ��ۤϴ�ñ. ��ʸ����ʸ���ζ��̤˱����ơ�
    * ���̤�����
        find ~/howm -name '*.howm' -print | xargs ruby -ne '$_ =~ /<<<\s+(.+)$/ and puts $1.split(/\s*<<<\s*/).join "\t"' | sort -u > ~/.howm-keys
    * ���̤��ʤ����
        find ~/howm -name '*.howm' -print | xargs ruby -ne '$_ =~ /<<<\s+(.+)$/ and puts $1.downcase.split(/\s*<<<\s*/).join "\t"' | sort -u > ~/.howm-keys

* ���
  * GNU Emacs �ʳ��ξ��:
    ��Ϥ褯�狼��ʤ��Τ�, �������Ƥ�������
    * meadow:
      ((<����Ѥ� Meadow|URL:http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=meadowmemo%20edition>))
      �ʤ�ǽ餫��Ȥ��ޤ�
      * cygwin + grep ���ѤΤȤ���,
        ���ǥ��쥯�ȥ�ȥ��ޥ�ɤ�ɥ饤�֥쥿��������ꤹ��.
        * ~/.emacs(.emacs.el ����) �Ǣ��Τ褦��
            (setq howm-directory "c:/cygwin/home/howm/")
        * cygwin ���鸫�� / �� emacs ���鸫�� / �������㤦�Ȥ�������.
    * xemacs:
      * font-lock �Υ�å����������������®���ʤ뤽��.
        thx > ((<�޸�����|URL:http://eron.info/k/diary/>))
          (setq font-lock-verbose nil)
    * Linux Zaurus:
      ((<Wiki|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?LinuxZaurus>))
      �򻲾Ȥ�������.
      thx > ((<100|URL:http://www.bookshelf.jp/2ch/unix/1063800495.html>))
  * SKK ��Ȥ����
    * .emacs �˰ʲ���񤤤Ƥ����ʤ���, Dired-X �� C-x C-j ��å���ޤ�
        (setq dired-bind-jump nil)
  * viper-mode ��Ȥ����
    * viper-mode ������ howm-mode ����ɤ��Ƥ���
      * post-command-hook �˰�������??
  * ���󥽡��� (emacs -nw) �ξ��
    * ������ɽ������ʤ�ü���ʤ�
        (set-face-foreground 'action-lock-face "blue") ;; �����Τ����˿��Ĥ�
  * ((<RD|URL:http://www.ruby-lang.org/ja/man/html/RD.html>))��Ȥ����
    * <<< �� RD �� include �Ȥ��֤�
    * �к���
      * include �ϻȤ�ʤ�. �ԤΤϤ���� <<< ��񤫤ʤ��褦��դ���.
      * include �ϻȤ�ʤ�. rd2 �򤫤������� howm2 -type=rd ���̤�.
      * ��󥯵�����ѹ�����
          ;; ��: .emacs (howm ���ɤ����)��
          (setq howm-ref-header "==>") ; goto ���
          (setq howm-keyword-header "<==") ; come-from ���
      * ((<�� howm wiki �Ρ�ʻ�ѥġ����|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?ExternalTool>))�⻲��

* ���Ǥ���ΰܹ� (ɬ���Хå����åפ�ȤäƤ���!) �� ((<URL:OLD.rd>))
  * ������ make install ���Ƥ�, �ĿͤΥ�˥塼�ե�������񤭹����Ϥ��ޤ���.
    ɬ�פʤ�, ��˥塼��ʬ���Խ����뤫,
    ja/0000-00-00-000000.howm ��ʬ�ǥ��ԡ����뤫���Ƥ�������.

=== �������ޥ���

����Ū�ˤ� M-x customize �� [Applications] �� [Howm] ��.
�Ԥ�Ȥ��ʤ����ܤ�, [Show] �Ǥ��꤬���ʴ����ͤ��������ǽ.

�����ˤʤ�����ˤĤ��Ƥ�, ~/.emacs (~/.emacs.el ����)��, �ʲ��Τ褦��ľ�ܽ�.
(��ä�����Ū�����Ť������, ((<URL:OLD.rd>))�򻲾�)

* ��
  * ���ƥХåե��� rd-mode �ʿ���Ĥ���
      ;; rd-mode.el ���ɤ߹��ޤ�Ƥ���Ȥ��������
      (setq howm-view-contents-font-lock-keywords rd-font-lock-keywords)

* ��������
  * �֥������ʡפǥ�˥塼, ��Ctrl-�������ʡפǿ������
      (define-key global-map [katakana] 'howm-menu)
      (define-key global-map [(control katakana)] 'howm-create)
  * [tab]([alt]-[tab])�Ǽ�(��)�Υ�󥯤˰�ư
      (define-key howm-mode-map [tab] 'action-lock-goto-next-link)
      (define-key howm-mode-map [(meta tab)] 'action-lock-goto-previous-link)
    * ����� tab �� C-i ��

* ��¸���
  * ����֤���/ǯ/ǯ����-��ʬ��.howm ��
      (setq howm-file-name-format "%Y/%Y%m%d-%H%M%S.howm")
    * �ե�����̾���Τ�ǯ���������äƤ��ʤ���, filter-by-date ����ǽ���ʤ�
  * 1 �� 1 �ե����� (����֤���/ǯ/��/ǯ_��_��.howm ��)
      (setq howm-file-name-format "%Y/%m/%Y_%m_%d.howm")
    * �Դ�������������ޤ�. �����Ǥ���ͤ����ɤ���
      * ���ñ�̤Ǥ���٤������ΰ������ե�����ñ�̤�
        (�����ȥ�ɽ��, ���������, ���ƤǤιʤꤳ��, uniq)
  * ������ɰ����� ~/howm/.howm-keys ���֤�
      (setq howm-keyword-file "~/howm/.howm-keys") ;; �ǥե���Ȥ� ~/.howm-keys
    * �������Ƥ�����, �㤦�ޥ���Ǥ� ~/howm/ �ʲ��Υ��ԡ������ǺѤ�.
    * ���Ǥ˽񤤤���⤬����ʤ�, mv ~/.howm-keys ~/howm/ �򤷤Ƥ�����,
      �ƹ��ۤ���(��((<���󥹥ȡ���>))).
    * �ǥ��å�: �������٤��ʤ�? (�δ��Ǥ���ۤɤ���, ��äƤߤʤ�������)

* ����
  * �����ǡ�!�פ����Ȥ��ν�����ޥ�ɤ��ѹ�
      (setq howm-view-summary-shell-last-file "_FILE_")
      (setq howm-view-summary-shell-hist
        '("mv _FILE_ ~/gomi" "touch _FILE_" "ls -l _FILE_"))
    * ������ޥ�ɤϡ�mv �ե�����̾ ~/gomi��
    * M-p �����Ƥ�����, ��touch �ե�����̾�פ��ls -l �ե�����̾��
  * �����Хåե��ο��Ĥ���
      (setq howm-view-summary-font-lock-keywords '(("^2003" . 'highlight)))

* ��˥塼
  * ��˥塼���ѹ�
    * ��˥塼�򳫤��� [menu �Խ�] ��ǥ꥿���� �� ��ͳ���Խ�
    * �褯�������ؤ� goto ��󥯤ʤɤ�񤤤Ƥ�������������
  * ��˥塼�ե�����ˡ�%recent�פ��%random�פȽ񤯤�,
    �ֺǶ�Υ��פ�֥��������������פΥ����ȥ����
    * �������ޥ���
        (setq howm-menu-recent-num 20)  ;; ɽ������Ŀ�
  * ��˥塼����ѿ���ؿ����ͤ�ɽ��
    * ��˥塼��ˤ����񤯤ȡ�
      * %here%foo     �� foo ���ͤ�ɽ��
      * %here%(foo 3) �� (foo '3) �η�̤�ɽ��
        * ��: %here%(howm-menu-search "�ۤ�") �� �֤ۤ��פθ�����̤�������
        * ������, ��Ͽ�����ؿ������Ȥ��ޤ��� (���ä��ʤ�����)
            (setq howm-menu-allow
                  (append '(foo bar) howm-menu-allow)) ;; foo �� bar �����
  * ��˥塼���ⰷ�����ʤ� (���������������оݳ���)
      ;; mv ~/howm/0000-00-00-000000.howm ~/hoge/fuga/menu.howm ���Ȥ��ơ�
      (setq howm-menu-file "~/hoge/fuga/menu.howm")

* ��äȷڤ� (cf. ((<�ٹ�Ū�ץ���ߥ�|URL:http://pitecan.com/fugo.html>)))
  * ��Ҥ� M-x customize �� [Howm Efficiency] �򻲾�
  * �ä�, �ܵ��ǻȤ��ˤ� howm-view-use-grep ������򤪤����ᤷ�ޤ�
  * Tips: gc-cons-threshold ���ͤ����䤹��®���ʤ��礬����.
    ref > ((<220,234-236|URL:http://www.bookshelf.jp/2ch/unix/1077881095.html>))
      (setq gc-cons-threshold (* 4000 10000))
  * Tips: grep-2.5 �Ǥ�, �Ķ��ѿ� LANG �� C �ˤ��Ƥ�����,
    �ޥ���Х����б������դˤʤä�®���ʤ�
    ((<ref|URL:http://search.luky.org/vine-users.5/msg06363.html>))

* ����
  * �оݥǥ��쥯�ȥ���ɲ�
    * ��ʸ�����ΤȤ�, ���˲ä��ƻ���ǥ��쥯�ȥ�ʲ���Ƶ�Ū��õ��
        (setq howm-search-path '("~/Mail" "~/News"))
        (setq howm-search-other-dir t) ;; �����Υȥ���ν���� (t �� nil)
    * M-x howm-toggle-search-other-dir ��,
      �嵭�ǥ��쥯�ȥ�򸡺��оݤˤ��뤫���ʤ����ȥ���
      * �����Х���ɤ�������гƼ��� (���󥿡��ե������Ϻ���ˤĤ���)

* ̤��¸�����ȰѺٹ��鷺, howm-mode �ʥХåե��򤹤٤ƶ���������륳�ޥ��
  (�������ᤷ�ޤ���. �Ȥ�ʤ��Ǥ�������.)
  * C-u C-c , Q
  * ��˥塼�˽񤯤ʤ� [��������]
  * ʪ���ʤΤ�, ����񤤤Ȥ��ʤ���̵��
      (setq howm-kill-all-enable-force t)

* �ƥ�ץ졼�Ȥ��ѹ���
  * ����ʤդ���
      Subject: �����ȥ�С��˻��פ�ɽ�� ��ľ���Υ꡼����������
      Date: Thu, 12 Sep 2002 15:45:59 +0900
      In-Reply-To: </home/hira/sawfish/rich-title/rich-title.jl> ��ľ���ե�����
      
      �� �� ��������
    * ~/.emacs ��
        (setq howm-template "Subject: %title\nDate: %date\n%file\n%cursor")
        (setq howm-template-date-format "%a, %d %b %Y %H:%M:%S %z")
        (setq howm-template-file-format "In-Reply-To: <%s>\n")
  * �ƥ�ץ졼�Ȥ�ʣ������
      ;; C-u 2 C-c , c �� 2 ���ܤΥƥ�ץ졼�Ȥǿ������
      ;; ��˥塼���� C-u 2 c �Ǥ�Ʊ��
      (setq howm-template
            '("= %title%cursor\n%date %file\n\n" "%date: %title%cursor"))
    * �Ĥ��Ǥ�, howm-template ���ͤ��ؿ��ʤ�
      ��universal-argument ��ľ���ΥХåե�������ˤ��Ƥ����Ĥ�Ƥ֡�
      �äƤ����Τ�Ź��ߤޤ���

* �񼰤��ѹ��� (howm-*.el �� load �������)
  * �����ȥ�(�����ڤ�) @@@ ��
      (setq howm-view-title-header "@@@")
  * goto ��� ==>��, come-from ��� <==��
      (setq howm-ref-header "==>")
      (setq howm-keyword-header "<==")
  * goto ��� ((��ġ�)), come-from ��� ((���ġ�))
      ;; ��䡧��Ⱦ�Ѥ�ľ���Ƥ�������
      (setq howm-ref-regexp "((��\\([^��\r\n]+\\)��))")
      (setq howm-ref-regexp-pos 1)
      (setq howm-keyword-format "((��%s��))")
      (setq howm-keyword-regexp "\\(((��\\)\\([^��\r\n]+\\)��))")
      (setq howm-keyword-regexp-hilit-pos 1) ;; �ִ�Ϣ������ɡ���
      (setq howm-keyword-regexp-pos 2)
      (setq howm-keyword-regexp-format "%s") ;; M-x describe-variable ����
    * ��: come-from ������ɤ� alias �Ǥ�,
      ���Τɤ��餫�������ꤷ�Ƥ��ޤ���.
      * �֡Ĥ����׷�: <<< foo <<< bar <<< baz
      * �֡Ĥ���Ĥޤǡ׷�: ((��foo��)) ((��bar��)) ((��baz��))
  * wiki ����� [[hoge]] �β������]]�פ�����
    * ��<<< hoge�פκ������, ��hoge�פˤⲼ��
        (setq howm-wiki-regexp "\\[\\[\\([^]\r\n]+\\)\\(\\]\\]\\)")
        (setq howm-wiki-regexp-hilit-pos 2)
        (setq howm-wiki-regexp-pos 1)

* ���ޤ���
  * ��������(C-c , d �ޤ��� [����])��ǯ����ά������, ��̤��פȲ��
      (setq howm-insert-date-future t)
    * �������ϻ��ΤߤǤ�. ��[2003-12-27]�׾�� RET �����Ȥ���ư��Ͻ���ɤ���.
  * ��http://�פǥ꥿���󲡤�����, URL �� kill-ring ��
      (setq action-lock-no-browser t)

* ͽ��ɽ��todo ����
  * ��ޥ��������(!+-~@.)���� RET ��ȯ�ǡֺѡפ�
      (setq howm-action-lock-reminder-done-default "")
    * ���ξ��, C-u RET �ǽ����ư�� (����󥻥�, �����ѹ�, ��)
  * ͽ��ɽ��todo ���������ޥ����������ľ�� RET �����Ȥ�,
    á������Хåե���ư save
      (setq howm-action-lock-forward-save-buffer t)
    * �ּ�ư save�פ��񹳤ʤ��������ɤ���
    * ��ư�� C-x s (̤��¸�Хåե������� save)�ʤꤹ��������ƻ����
  * ��α�������ϰ�
      (setq howm-todo-priority-defer-init -14)  ;; ����� = ����
      (setq howm-todo-priority-defer-peak 0) ;; ���
  * !+-~. �ν��٤Υ������ޥ���
    * ��: ��˥塼��, �����������ɽ���סֺѤ�ɽ����
        (setq howm-menu-todo-priority -50000)
        (setq howm-todo-priority-done-bottom -44444)
    * howm-todo-priority-normal-bottom ��. ������(howm-reminder.el)����.

* action-lock
  * { } (�ȥ��륹���å�)���ѹ�
      ;; howm �� load ����
      (setq action-lock-switch-default '("{ }" "{*}" "{-}")) ;; ���ĤǤ�
  * {_} (̤����)���ѹ�
      (setq howm-dtime-format "[%a %b %d %H:%M:%S %Y]") ;; {_}
      (setq howm-template-date-format "[%Y-%m-%d %H:%M]") ;; �ƥ�ץ졼��
  * ��file://�ġפ��http://�ġפ��ѹ� (�ޤ��������罸)
    thx > ((<945|URL:http://www.bookshelf.jp/2ch/unix/1063800495.html>))
      ;; howm (���Τˤ� action-lock.el) �Υ��ɤ������.
      ;; ��file://��
      (setq action-lock-open-regexp
            "\\<file://\\(localhost\\)?\\([-!@#$%^&*()_+|=:~/?a-zA-Z0-9.,;]*[-!@#$%^&*()_+|=:~/?a-zA-Z0-9]+\\)\\>")
      (setq action-lock-open-regexp-pos 2) ;; 2 ���ܤΡ�\\(��\\)�פ��ե�����̾
      ;; ��http://��
      (setq action-lock-browse-regexp
            "\\<\\([htp]\\{3,5\\}s?\\|ftp\\)://\\([-!@#$%^&*()_+|=:~/?a-zA-Z0-9.,;]*[-!@#$%^&*()_+|=:~/?a-zA-Z0-9]+\\)\\>"
      (setq action-lock-browse-regexp-pos 0) ;; �ޥå��������Τ� URL
  * action-lock �ɲ���:
    ��Message-ID: �ġפǥ꥿���󲡤�����, �����᡼��� namazu �Ǹ���
      ;; howm �� load �������
      (defun my-howm-search-message-id (id)
        (message "Searching...")
        (let* ((query (format "+message-id:%s" id))
               (args `("-l" "-n" "1" ,query "/home/hoge/NMZ/Mail"))
               (found (car (howm-call-process "namazu" args))))
          (if found
              (progn
                (find-file found)
                (re-search-forward "^$" nil t)
                (message "Done."))
            (message "No match."))))
      (setq action-lock-default-rules
            (cons (action-lock-general 'my-howm-search-message-id
                                       "Message-[Ii][Dd]: \\(.*\\)$"
                                       1)
                  action-lock-default-rules))

* ��˥塼�򹹿����뤿�Ӥ�, ���������ؤ� export �⹹�� (��((<�����ġ���>)))
    (defun my-howm-menu-hook ()
      (shell-command "tag2plan ~/howm/*/*/*.howm > ~/.dayplan_tag &")
      (switch-to-buffer howm-menu-name))
    (add-hook 'howm-menu-hook 'my-howm-menu-hook)

* ((<RD|URL:http://www.ruby-lang.org/ja/man/html/RD.html>))��Ȥ����:
  ��Ƭ�� * �ǥ���ȥ�γ��Ĥ��Ǥ���褦��
  �� ((<237-238|URL:http://www.bookshelf.jp/2ch/unix/1063800495.html>))

* ���ޤ�
    (setq howm-congrats-format
          '(
            "(�����ء���) %s"
            "(�����ء���) %s"
            ;; �İʲ�ά��
            ))

* ��äȤ���������ˤ�, *.el ��Ƭ�򻲾�

=== �����ġ���
(Ʊ���ġ���� ext/ ��)

* HTML �ؤ��Ѵ�: howm2 (Ʊ��. �� ruby)
  * ��
    * ���ǥ��쥯�ȥ� ~/howm/ ���Ѵ����� ~/converted/ ���Ǥ�
        ./howm2 ~/howm/ ~/converted/
    * <<< ����ʸ����ʸ����̵��
        ./howm2 -i ~/howm/ ~/converted/
    * ��󥯽񼰤λ���
        ./howm2 -comefrom='<<<' -goto='>>>' ~/howm/ ~/converted/
    * �֤ۤ��פ�ޤ�ե�������� HTML ��
        grep -rl '�ۤ�' ~/howm/ | howm2 -list ~/converted/
  * ���⹩�פ��Ƥʤ��Τ�, ���٤��ĥ���ɤ�����
  * alias �ΡֺƵ�Ū�ʡ�Ÿ����̤���ݡ���

* �������� & todo ����: hcal.rb (Ʊ��. �� ruby)
  * ��������(ͽ�ꡦ���ڡ��Ѥߤΰ���)�����
      hcal.rb -schedule_mark='��' -deadline_mark='��' -done_mark='��' ~/howm/*/*/*.howm
    * ����ʴ����Ǥ��餺��
        ----------------<6>---------------- 2003
        01 Sun 
        02 Mon ������������Ϣ�� ��B4�ع� ���� �����ش��ü¸� 12:40 <<<<##>>>>
        ��
    * ���ϡ���(@[2003-06-02]!), ����ͽ��(@[2003-06-02]@), ���Ϻ�(@[2003-06-02].)
    * <<<<# �ϡֺ�����, #>>>> �ϡ���ǯ��Ʊ��Ʊ����
      * ����ʴ����� alias ���Ȥ�������
          alias hcal="hcal.rb -schedule_mark='��' -deadline_mark='��' -done_mark='��' ~/howm/*/*/*.howm | less '+/<<<<#'"
  * �ֽ��ٽ� todo �����פ����
    (howm ��Ȥ��ʤ�����. ChangeLog �ɤʿͤؤΤ��ޤ��Ǥ�)
    * ���ޥ�ɥ饤���
        hcal.rb -l memo.txt
    * emacs ���� M-x grep ����
        Run grep (like this): hcal.rb -l ~/memo/*.txt

* �վ�񤭻ٱ�:
  * ((<org-mode �Ȥ�ʻ��|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?OrgMode>))

* �ʰ׾�����Ģ
  * ������ɤ����, ������˽񤤤Ƥ���
      $����$ 500�� �顼���
  * ��<<< $����$�פʤ��>>> $����$�פʤ�ǰ�����ɽ��.
    �ʤ���ߡ������Ȥ����ϰϻ���.
  * M-x yen-region ��, �֢����ߡפ���
    �� ((<yen.el|URL:http://howm.sourceforge.jp/a/yen.el>))

* ((<�� howm wiki �Ρ�ʻ�ѥġ����|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?ExternalTool>))�⻲��

== ����

=== �����ˤĤ���

* �ե����볫������Ӥ˥������äƤ����°׼���
  * ~/.howm-keys �˥�����ɤΰ���
  * �ե�����򳫤��Ȥ��ϡ�
    * .howm-keys �γƥ�����ɤˤĤ���, �и���̵ͭ�򸡺�
    * �и�������ɤ� or �ǤĤʤ�������ɽ�������
    * ��������ɽ���� font-lock �� action-lock ������
  * �ե�������¸�������Ƥ򥹥���󤷤�, ~/.howm-keys �򹹿�

* ����
  * ���ǥ��쥯�ȥ� ~/howm/ �ʲ���Ƶ�Ū��������.
    �ե�����̾���ĥ�Ҥ� ~/howm/ �ʲ��Υǥ��쥯�ȥ깽����, �ɤ��Ǥ�褤.
    * �ե�����̾�������, 
      * �ե�����̾��ǯ���������äƤ��� (filter-by-date �Τ���)
      * string<= �ǥ����Ȥ�����������ˤʤ�
  * �ߴ��ʸ����ؿ��������Ѱ�. ����������Ȥ���.
    * real-grep (grep ��Ƥ�)
    * fake-grep (elisp �Τ�)

* �ե����빽��
  * howm ���ΤȤ���Ω
    * bcomp.el
      * make ���˻Ȥ�����
      * navi2ch-cvs-0.0.20031209 �������
    * cheat-font-lock.el
      * font-lock-keywords ��夫���ѹ����뤿��δؿ�
      * font-lock.el �����������˰�¸
    * action-lock.el
      * action-lock-mode (minor-mode)
        * ��ʸ(����ɽ��)����ˡ(�ؿ�)���Ȥ���Ͽ
        * �꥿���󥭡�á������
          * ��ʸ�ξ� �� ��ˡ��ȯư
          * ����ʳ� �� ����Υ꥿���󥭡�
    * riffle.el
      * riffle-{summary|contents}-mode
        * ���������ƤΤѤ�Ѥ�ɽ��, ���Ƥ�Ϣ��ɽ��
        * �����Ǥ�, post-command-hook �ǰ�ư���� �� ����ɽ���򹹿�
        * �Хåե��������ѿ� riffle-item-list �˹��ܤ��ݻ�
      * gfunc.el �����
    * gfunc.el
      * ��ľ generic function
    * illusion.el
      * illusion-mode (minor-mode)
      * �դĤ��Ρ֥ե�����פǤʤ��оݤ�, �������Խ�������¸
      * ���ΤȤ�����Ѥ���Ƥ��ʤ�
    * honest-report.el
      * �Х���ݡ��Ȥ�����
  * howm ����
    * ����
      * howm-backend.el
        * �Хå�����ɤ�ʬΥ
        * ��ݲ�
          * �ǥ��쥯�ȥ� �� folder
          * �ե����� �� page
          * �ޥå��ս� �� item
      * howm-view.el
        * howm-view-{summary|contents}-mode (major-mode)
          * riffle-{summary|contents}-mode ��������
          * �����μ¹�
      * howm-mode.el (howm-mode-mode.el �����̾[2004-07-14])
        * howm-mode (minor-mode)
          * ��ҤΥ������ʤ�
    * ����
      * howm-date.el
        * �������Ϥλٱ�
      * howm-reminder.el
        * ������ todo
      * howm-menu.el
        * howm-menu-mode (major-mode)
    * ����
      * howm-version.el
        * ��� howm-version �����ꤹ�����
      * howm-vars.el
        * defvar, defcustom, ��
      * howm-lang-*.el
        * �����¸���ѿ�
      * howm-menu-*.el
        * �����˥塼�ե���������Ƥ�ʸ��������Ȥ������
      * howm-mkmenu.el
        * howm-menu-*.el �� ja/0000-00-00-000000.howm �������������륹����ץ�
        * ��԰ʳ��ϻȤ�ɬ�פʤ��Ϥ�
    * ��
      * howm-cl.el
        * cl �ѥå������ؤΰ�¸��ޤȤ᤿����
      * howm-common.el
        * howm-*.el �� require
        * �ä�, �ե�����ޤ����ǻȤ��ޥ���Ϥ����� (�� byte-compile �к�)
      * howm-misc.el
        * ��
      * howm.el (howm-mode.el �����̾[2004-07-14])
        * �ᥤ��ե�����. require �������.

=== ư���ޤ����?

(�Х��λ�Ŧ�򤯤���������)

* �ʲ��Τ褦�ˤ��Ƥ���������, Ĵ�����䤹���ʤ�ޤ�
  * �Ǥ������ make test �򤪴ꤤ���ޤ�
      cd howm-��.��.��
      make test
  * win �ʤ�, test.bat �򤪴ꤤ���ޤ�
    * test.bat ��Ρ�HOWM_EMACS=�ġפ�Ķ��ˤ��碌�ƽ���
    * test.bat ��¹�
  * �ɤ����, emacs ��Ω��������, ����ɼ��ɽ������ޤ�
  * ((<�ʤ�Ǥ虜�虜? �� �Х���ݡ���FAQ|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportFAQ>))

* ��­: ��������ľ���
  * �ֻ��ͤ��סִ��ΤΥХ����פΥ����å��ä�, ���ä����Ǥ���ͤ�.
  * howm �˴ؤ��Ƥ�, ���Υ����å������פǤ�.
    �������, ���ڤˤɤ�ɤ��Ŧ���Ƥ��������������꤬�����Ǥ�.
  * ����, ��Ԥ��ܤ��Ϥ��Ȥ���(2ch �� howm wiki)�ˤ������Ǥ�������.
  * cf.
    ((<�Х���ݡ���FAQ|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportFAQ>)),
    ((<YASWiki:�����ץ󥽡����ϲ�Į����|URL:http://web.archive.org/web/20041018232953/http://nnri.dip.jp/~yf/cgi-bin/yaswiki.cgi?name=%A5%AA%A1%BC%A5%D7%A5%F3%A5%BD%A1%BC%A5%B9%A4%CF%B2%BC%C4%AE%B5%A4%BC%C1>))

* ��Գн�
  * �ǥХå����ѿ� howm-call-process-last-command
  * C-u M-x howm-bug-report �Ǵ�Ϣ�ѿ��ΰ���
  * M-x howm-elp ��, �ץ�ե����� elp �ν���

== ����

=== ����

((<���ͥ�|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?IdeaSource>))����.
�ä�, Q-pocket��HashedWiki��ChangeLog ��⤫�餤�äѤ��ޤͤ��Ƥޤ�. ����.

* ����ͺ�����֤䤵���� Emacs-Lisp �ֺ¡�(���åȥ����ƥ�, 1999)
  ISBN 4-906391-70-2
  �� 
  ((<����饤���� (��?)|URL:http://www.gentei.org/~yuuji/elisp/>))
  * elisp �Ϥ����ʤ鰵��Ū�ˤ�������
  * 6.4 ��������Ρ֥������� dired�פ򻲹ͤˤ����Ƥ��������ޤ���

=== ������Ͽ

thx > patch�����ɰơ���Ŧ�򤯤����ä�����

* ��꡼���� howm-1.3.9.2 [2011-12-28]
  * ���ޤ��� fix (howm-1.3.9.2rc4 ��Ʊ���ƤǤ�)
    * mac �� grep ���ѻ��˥��顼���ФƤ���
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/787-790n>))
      ((<thx|URL:http://hibari.2ch.net/test/read.cgi/unix/1141892764/898-901n>))
      * grep �Υǥե���ȥ��ץ��������ꤹ������,
        --exclude-dir ���̤뤫��ǧ����褦�ˤ��ޤ���.
    * �礭���ե졼��ǰ���ɽ���򤷤��Ȥ���;�פʥ�����ɥ�ʬ�����
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    * howm-vars.elc ���ɻ��ηٹ��old-style backqoute detected!�פ����.
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))

* ��꡼���� howm-1.3.9.1 [2011-01-02]
  * fix: emacs-24.0.50 �ǤΥ��顼
    (Symbol's function definition is void: make-local-hook).
    thx > ���� ���� ���� (munepi at vinelinux.org)

* ��꡼���� howm-1.3.9 [2010-12-30]
  * Note
    * �ۤȤ���ѹ��Ϥ���ޤ���, �ޤ���ǯ�ۤɤ��ä��Τǥ�꡼�����Ƥ����ޤ�.
    * howm-test100702 �Ȥΰ㤤��, �ɥ�����Ȥ��������� ext/tag2plan �κ������.
    * ���Υ�꡼�����Ѥ����, �ǥե����������Ѥ���
      ������ǽ����������������Τ�Τ� howm-1.4.0 �Ȥ��ƥ�꡼������ͽ��Ǥ�.
  * fix
    * ��˥塼���� y ����(�ޤ��� [ͽ��] ��� RET)��ͽ��ɽ�򳫤����Ȥ�,
      ���ƥХåե�������������֤Υ��ˤʤäƤ��ʤ��ä�.
      ((<thx|URL:http://pc12.2ch.net/test/read.cgi/unix/1141892764/826>))
    * �ɥ�����ȤθŤ��ս���ľ��
    * ���Ƥ���Ƥ��ʤ��ġ���(ext/tag2plan)����

* ��꡼���� howm-1.3.8 [2009-12-31]
  * Note
    * �礭���ѹ��Ϥ���ޤ���, ��ǯ���ä��Τǥ�꡼�����Ƥ����ޤ�.
    * howm-test090723 �Ȥΰ㤤��, howm-excluded-dirs �� ".git" ��
      �ɲä��������Ǥ�.
  * �ѹ�������
    * �᤮�����ڤ��忧
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/738>))
    * _darcs/ �ʤɤ򸡺��оݳ��� (howm-excluded-dirs).
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/708n>))
      * ���ޤɤ��� GNU grep ��ȤäƤ���ʤ颭�⤷�Ƥ�����̵�̤ʸ�����ʤ��ޤ�.
          (setq howm-view-grep-option "-Hnr --exclude-dir=_darcs")
      * �����, *.howm ������������褦�ˤ�������С�
          (setq howm-view-grep-option "-Hnr --exclude-dir=_darcs --include=*.howm")
    * ((<yagrep|URL:http://www.kt.rim.or.jp/~kbk/yagrep/index.html>)) �Ȥ�
      �ߴ����Τ���, grep �ƤӽФ����˥ǥ��쥯�ȥ�̾������ / ����.
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/685-686n>))
    * ((<HidePrivateReminder|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?HidePrivateReminder>))
      �Τ���������򾯤��ݽ�.
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/731>))
  * fix
    * C-c , l (howm-list-recent)���˳����ե����뤬¿������ȥ��顼.
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/733>))
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Windows>))
      * howm-view-use-grep �����ꤷ�Ƥ������ȯ��.
        meadow ���� "Spawning child process: exec format error" �ˤʤ�餷��.
      * grep �ƤӽФ����Υ��ޥ�ɥ饤�� howm-command-length-limit �ʾ��
        Ĺ���Ȥ���ʬ�䤷�ƸƤӽФ��褦ľ���ޤ���.
    * (setq howm-list-title t) ���Ƥ�����,
      come-from ��󥯾�� RET �����Ȥ��⥿���ȥ������ɽ������褦��.
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/762>))
      * �⤷������ư��褱���,
        M-x customize-variable RET howm-list-title RET ��,
        howm-keyword-search �ʳ��򤹤٤ƥ��åȤ��Ƥ�������.
    * ������¸�����Ȥ��˥�˥塼����ư��������ʤ��ʤäƤ���.
      (howm-menu-expiry-hours ���������ꤷ�����Τ߳���)

* ��꡼���� howm-1.3.7 [2008-12-31]
  * Note
    * ����Ū�ʥ�����������, ���ޤ��޲��ɡ�����
    * howm-1.3.6 (�⤷���� howm-test080531) ������
      �ѿ� howm-list-normalizer �����ꤷ�Ƥ�������,
      ���������ߤ�, �ѿ� howm-normalizer �����ꤷ�Ƥ�������
      * ��ư�ɤߤ���������ߤƤϤ��ޤ�����
    * howm-1.3.7rc4 �Ȥۤ�Ʊ����ΤǤ�
      * ���ܰդ� "Wrote ..." ��ɽ�������Х���ľ���ޤ���
  * �ѹ�������
    * ��ߴ����ѹ�
      * ���ѿ� howm-list-normalizer ���鿷�ѿ� howm-normalizer ��
        * �ܹ���ˡ
          * M-x customize �����ꤷ�Ƥ����ʤ�,
            M-x customize-variable howm-list-normalizer RET ��
            ��Off�פ����ꤷ,
            M-x customize-variable howm-normalizer RET �ǲ�������ꤷ�ʤ���
          * .emacs ���� (setq howm-list-normalizer 'howm-view-sort-by-����)
            �����ꤷ�Ƥ����ʤ�, ���Τ褦�˽񤭤�����
              (setq howm-normalizer 'howm-sort-items-by-����)
          * (setq howm-list-normalizer �Ĥ���ʳ��β�����)
            �����ꤷ�Ƥ����ʤ�,
            * lisp ���狼����:
              ���λ����ѹ��ˤ��碌�ƽ�������
              * ���ѿ�: �ָ��ߤΰ������¤٤�����ɽ����ľ���ؿ��פ����
              * ���ѿ�: ��Ϳ����줿�ꥹ�Ȥ��Ф�, �����¤٤������֤��ؿ��פ����
            * lisp ���狼��ʤ���:
              ((<2ch UNIX �� howm ����|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/l50>))
              ��
              ((<howm wiki �Ρ֤ʤ�Ǥ��|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Comment>))
              �Ǥ����̤�
        * �⤷���ѿ��򥻥åȤ����ޤޤˤ��Ƥ����ȡ�
          * ñ����ɤߤ�����줽���ʤ�, ���ѿ����ɤߤ����ƿ�������¹�
          * �ɤߤ������ʤ��ä���, �������¹� (���Ψ)
      * �ֺ��������������աפ� [YYYY-MM-DD] �Ǥʤ� YYYY-MM-DD ���忧
        ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/275>))
        * �������˥塼�Υե�����̾��, �ޥå�������忧
        * ���Τ褦���ᤷ������С�
            ;; ���������������դ�, [YYYY-MM-DD] �η��������忧
            (setq howm-highlight-date-regexp-format (regexp-quote "[%Y-%m-%d]"))
    * �����Хåե�
      * ������������Ū�ʰ����Хåե���ɽ��������
      * �����Хåե������ X (dired-x) ����, ����������б��ե�����̾���֤�
        ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>))
        > 797 ����
        * 1.3.2 �α�����ǽ������� & �ǥե���Ȳ�.
          �ѿ� howm-view-dired-keep-cursor �Ϻ�����ޤ���.
      * howm-view-summary-previous-section ��ֳƥե�����κǽ�Υҥåȹԡפ�
        �ߤޤ�褦�ѹ�
        ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/360>))
      * ���ƥХåե��ǰ쥢���ƥ����ɽ�����Ƥ���Ȥ���,
        ���ڤ�����====>>> xxx.howm�פ������ʤ�.
        ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/314>))
      * �����Хåե��Υ����ȴ��� summary-match-string ���ɲ�
        * ���ꤷ������ɽ���˥ޥå�����ʸ����ν�˥�����
          * ��: ��2006-06-..�פ���ꤹ���, 2006ǯ6��ι��ܤ����ս��
        * ���ʤߤ�, summary-match ��, �ޥå�������Τ��̤ˤ�äƤ������
          * �ޥå�������Τɤ����ξ岼��ӤϤ��ʤ�
    * ��˥塼
      * ��˥塼�� %recent �� %random �Ǥ�ե�����̾���夽��.
        ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2007/000032.html>)) > Mielke-san (peter.mielke at gmail.com)
        * �ѿ� howm-menu-list-format �� %recent ����� %random �Ѥ�
        * ���ѿ� howm-menu-reminder-format �� %schedule ����� %todo ��
      * ��˥塼�� %random% ��, Ʊ���ե����뤫��ϰ���ܤ������Ф�ʤ��褦��
        ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/563-566n>))
      * ��˥塼������ɽ����ꥹ�Ȥǻ��ꤹ��褦�ѹ�.
        �Ѹ�ɽ���Υǥե���Ȥ� "Sun" ����ľ����.
        ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/147>))
          (setq howm-day-of-week-ja '("��" "��" "��" "��" "��" "��" "��"))
          (setq howm-day-of-week-en '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
        * �äƤ�����, �虜�虜�ȼ������������
          (format-time-string "%a") ����Ǥ��Ǥ⹽��ʤ�?
      * �����˥塼�˥ܥ�����������ɲ�.
        ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/144>))
    * ������
      * howm-view-grep-option ��ʣ���Υ��ץ�����񤱤�褦��.
        ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/458>))
          (setq howm-view-grep-option "-Hnr --include=*.howm") ;; *.howm �Τ߸���
        * ñ��� split-string ���Ƥ����.
          ��äȤޤ���ʤΤ�ɬ�פʤ餪�Τ餻��������.
      * ñ������椬���ޤ��� come-from ������ɤ˰��פ��ƤⲼ��������ʤ�����.
        ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2007/000030.html>)) > Mielke-san (peter.mielke at gmail.com)
          ;; ASCII ʸ���ΤߤΥ�����ɤ�, ñ������˥ޥå����ƤⲼ��������ʤ�
          (setq howm-check-word-break "^[[:ascii:]]+$")
      * ͽ��ɽ, ToDo �ꥹ�Ȥˤ�Ķ��������ɽ��.
        ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2006/000028.html>)) > Mielke-san (peter.mielke at gmail.com)
      * .howm-history �ޤ��ε�ư�����.
        ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/179>))
        ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/193-194n>))
        * �Хåե�������ɽ�����ʤ�
        * "Wrote ..." ��ɽ�����ʤ�
        * make test ���� ~/.howm-history ������ʤ�
  * fix
    * howm-menu-categorized-reminder ��ɽ������ʤ����ܤ����ä�
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    * (setq howm-view-list-title-type 2) �ΤȤ� C-c , a �ǥ��顼
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/552>))
    * �����ȥ�Τʤ���⤬ C-c , a ��ɽ������ʤ��ä�
    * howmoney ���Ȥ��ʤ��ʤäƤ���.
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/503>))
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/514>))
    * ͽ��� todo ����Ĥ�ʤ��Ȥ���ͽ��ɽ�� todo �ꥹ�Ȥ�ƤӽФ������.
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/494>))
    * ͽ��ɽ�� todo �ꥹ�Ȥ� action-lock-mode �����ܰդ˥ȥ���.
    * howm2, howmkara �� -exclude ���ץ������Զ��.
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>)) > dareka ����
    * ((<HidePrivateReminder|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?HidePrivateReminder>))�� C-c , t ����No match�פ�
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?HidePrivateReminder>)) > taku ����
      * �ߴ�������
    * howm-occur �ǰ����Хåե��θ����줬�ϥ��饤�Ȥ���ʤ��ʤäƤ���
    * �֡��� �ƥ��� ���� Test�פΡ�Test�׾�� RET ��á���Ƥ�֥ƥ��ȡפ�
      ��������ʤ��ä�
      * howm-keyword-case-fold-search �򥻥åȤ��Ƥ����Ȥ�����ʸ����ʸ�������
    * C-c , l �Ǥ����������դ�ʹ���ʤ��褦��
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/340>))
      * [2007-04-02] �˺�������Х�
    * ������̤ΰ����ǡ֡��� �ۤ��פ���Ƭ�ˤ��ʤ���礬���ä�
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/328>))
      * (setq howm-list-title t) ���Ƥ����ȯ��
      * remove-duplicates �λ��ͤ�褯�Τ�ʤ��ä�����. �ٶ��ˤʤ�ޤ���.
    * (setq howm-list-title t) ���Ȱ����Хåե�����������Ƥ�ɽ�������Х�
    * howm-view-contents-limit �������Ƥ��ʤ��ä�
    * ���դǤιʤ���߷�̤�����ʬ¿������
    * narrowing ��Ϣ���Զ��(widen ȴ��)
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/160-161n>))
    * ��˥塼�Ρ�%reminder�פ����, �᤮��ͽ�꤬ɽ������Ƥ���.
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/149>))
    * ��˥塼��Ρ�> 2006-07-26-015606 | �ġפΡ�7�׾�� RET ��á���ȥ��顼.
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReport>)) > na ����
      * �ѿ� howm-menu-list-regexp ����������ä�ľ��������
    * �ۤʤ�ǥ��쥯�ȥ��Ʊ̾�ե����뤬����ɽ���Ǻ�Ʊ����Ƥ���.
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?BugReportPaste>))
    * howm-view-split-horizontally �� t �Ǥ� nil �Ǥ�ʤ��Ȥ������̤�ư����ѻ�.
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/591>))
      * howm-1.2 �� 1.3.7rc2 �ǲ���Ƥ�����, �Х���ݡ��Ȥʤ�.
        ���ä�ï��ȤäƤ��ʤ� ^^;

* ��꡼���� howm-1.3.6 [2008-05-31]
  * fix: 2008-02-24 �ʹߤ� CVS ��ü emacs ��, ¾�Хåե����忧������.
    ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/484-487n>))
    * minor mode �� font-lock-keywords-only ��ľ�˿���ΤϹԵ�����?
    * howm-test20080514 ����ΥХå��ݡ���
  * (howm-1.3.6rc1 ����Ȥ�Ʊ���Ǥ�)

* ��꡼���� howm-1.3.5 [2007-12-09]
  * fix: �ƻ��ֺǽ�����������ͽ�꤬��˥塼��ɽ������ʤ��ä�.
    ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2007/000034.html>)) > Mielke-san (peter.mielke at gmail.com)
    * howm-test071108 ����ΥХå��ݡ���
  * fix: ����Ʊ�ǥХ��ȥ���ѥ���Ǥ���褦��
    * Wanderlust �� Navi2ch �򻲹ͤˤ���,
      ����¸�ΰ�����(require �ν���)����
    * howm-test07-05-18 ����ΥХå��ݡ���
  * automake �� 1.6 ���� 1.10 ��
    * howm-test07-05-05 ����ΥХå��ݡ���
    * automake-1.10 �� elisp-comp ���Ȥ���褦�ˤʤä�
  * (howm-1.3.5rc1 ����Ȥ�Ʊ���Ǥ�)

* ��꡼���� howm-1.3.4 [2006-12-16]
  * �������ƥ�����
    ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/207>))
    * ��������?
      * Emacs �ˤ�, �ե����뤴�Ȥ˥������ѿ���ư���ꤹ�뵡ǽ������ޤ�.
        ������Ѥ����, howm ���ѻ���Ǥ�դ�̿���ư�¹Ԥ����뤳�Ȥ��Ǥ��ޤ�.
        ((<ref|URL:https://www.codeblog.org/blog/ueno/20060118.html>))
    * �ɤ�ľ����?
      * howm ��Ϣ��������ܥ�� risky-local-variable °���򥻥åȤ�,
        ��Ҥμ�ư������˥����å�������褦�ˤ��ޤ���.
    * �С�����󥢥åפ������ʤ�/�Ǥ��ʤ��ΤǤ���?
      * ���������Խ�����ǽ�ʤ�,
        howm.el �������˰ʲ��Υ����ɤ�ä���Τ��μ¤Ǥ�.
        �Х��ȥ���ѥ���Τ��ʤ����⤪˺��ʤ�.
          ;; howm-1.2.2 �ʹ���. howm ��Ϣ��������ܥ�� risky-local-variable °��.
          (mapcar (lambda (symbol) (put symbol 'risky-local-variable t))
                  (howm-symbols))
      * ���줬����ʾ��� .emacs �˰ʲ���ä��Ƥ�������.
          (eval-after-load "howm"  ; �� autoload/load/require �ε��Ҥˤ��碌��
            ;; howm-1.2.2 �ʹ���. howm ��Ϣ��������ܥ�� risky-local-variable °��.
            '(mapcar (lambda (symbol) (put symbol 'risky-local-variable t))
                     (howm-symbols)))
      * �ɤ���ˤ���, ������ȿ�Ǥ��줿���Ȥ򤴳�ǧ��������.
        * emacs ��Ω���夲ľ��, howm ��ư
        * �ʲ��� *scratch* �Хåե���Ž��, �Ĥ���̤θ�˥���������֤��� C-j ��
          ����
            (get 'howm-version 'risky-local-variable)
        * t ��ɽ�������� OK
    * �������ѿ��μ�ư����򤢤��ƻȤ������Ȥ���?
      * �ʲ��Τ褦���ѿ����Ȥ˲�ؤ��Ƥ�������.
          ;; ��: �ѿ� howm-auto-narrow �ϥե����뤴�Ȥμ�ư��������
          (put 'howm-auto-narrow 'risky-local-variable nil)
    * howm �˸¤餺, �������ѿ��μ�ư�������ڻȤ��ʤ�����ˤ�?
      * .emacs �˰ʲ���ä��Ƥ�������.
        ������ emacs �ΥС������ˤ�äƤ��Դ������⤷��ޤ���.
        ((<ref|URL:http://www.kmc.gr.jp/~tak/memo/emacs-local-variable.html>))
          ;; �������ѿ��μ�ư����򥪥�
          (setq enable-local-variables nil)
  * fix: CVS ��ü emacs �ǥ�˥塼�ʤɤ˿����Ĥ��ʤ�
    ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/165-169n>))
    * ������, cheat-font-lock-20040624-format-p �������� = �� >= ��ľ������
    * howm-test061015 ����ΥХå��ݡ���

* ������ǽ (experimental)
  * 1.1.1.* ��������
    * �����������ɤ��ɤ߽񤭤� howm ��
      * ((<GNU global|URL:http://www.tamacom.com/global-j.html>))
        (((<��|URL:http://www.tamacom.com/tour/lang/ruby/S/21.html>)))
        ��ɤ��� on the fly �Ǥᤶ����
      * �ޤ���ȯ��. ̣������ˤϡ�
        * �ѿ� howm-configuration-for-major-mode ������
          * major-mode �˱�����, come-from ������ν񼰤��Ѥ���
          * howm-misc.el �Υ����Ȼ���
        * M-x howm-open-directory-independently ���� ~/elisp/howm �ʤɤ�����
      * ���ΤϷ�� grep �ʤ������, ���ޤ긭��ư�����Ԥ��ƤϤ����ʤ�
        * elisp, tex �Ǥ�����������, ruby ����Ȥ���Τˤʤ餺.
          * �� elisp �δؿ�̾�� tex �Υ�٥�����Ū�˰��. ruby ����.
  * 1.2
    * �����������ƥХåե��˥ե��������Τ�ɽ��������
        (setq howm-view-preview-narrow nil)
      * Ϣ����Ͻ���ɤ���(�����ڤ���ϰϤΤ�)
      * howm-configuration-for-major-mode �ʳ��ǻȤ����̤�, �ޤ��ʤ��Ǥ��礦
    * ��ޥ����
      * ��ޥ�����Υ������ޥ���
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/296>))
          ;; ���������Υ�ޥ������*�פ����������:
          ;; ��[2004-07-11]* �ۤ��פ�, �������ޤǾ徺��, ���Τ��Ȳ���.
          ;; (���� = - |�٤� / ͱͽ����|. ͱͽ�����Υǥե���Ȥ� 3)
          ;; 1. ���ٴؿ������ (�٤��ͱͽ����(�ȥ����ƥ�)�򿩤äƽ��٤��Ǥ�)
          ;;    �٤�: ���������麣���ޤǤ�����. �ޤ��ʤ�ޥ��ʥ�.
          ;;    ͱͽ: ��[2004-07-11]*8�פʤ� 8. ��[2004-07-11]*�פ����ʤ� nil.
          ;;    ����: �礭���ۤɾ�. �ֳн�פʤ������ 0 ������ 1 ���ĸ���.
          ;;    (�����ƥ�: �դĤ��ϻȤ�ʤ����ɤĤ��Ǥ�. howm-backend.el ����)
          (defun my-priority (late lazy item)
            (let ((r (howm-todo-relative-late late lazy 3)))
              ;; r = late / lazy. ̵������� lazy = 3.
              (- (abs r))))
          ;; 2. face �����
          (defface my-face '((t (:foreground "cyan"))) "my face")
          (setq my-face 'my-face)
          ;; 3. ����, ���ٴؿ�, face ����Ͽ.
          ;; �Ĥ�ΰ�����Ĥ�, ��ͽ��ɽ��ɽ�����뤫�ס�todo �ꥹ�Ȥ�ɽ�����뤫��.
          (howm-define-reminder "*" #'my-priority 'my-face nil t)
        * ����: ��¸�ν��ٴؿ��Υ���դ�
          ((<UNIX USER ��ε���|URL:http://howm.sourceforge.jp/uu/#label:11>))
          �˽ФƤޤ�
        * �Х�
          * �����ε���Ϥ������� (����ɽ�� [��] ������̤ʰ�̣����ĵ���ϡ�)
          * ��[2004-07-11]- �ۤ��פ����-�׾�� RET ���ơ�*�פ����Ϥ���ȥ��顼
        * �Ȥꤢ����á����. ����ʤ�Ǥ�����Ǥ��礦��?
      * �����񤱤�
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/141>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/148>))
        ((<thx|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/597>))
          [2004-07-16 10:15]@ ���
        * ���ΤȤ���񤱤����. ���̤ϲ���ʤ�.
        * ���夳�������˳�ĥ���뤫��̤��
    * ���շ���
      * ���վ�� RET��2 ���Ƥ����
          -, + �� ����, ����
          (, ) �� ����, ����
          {, } �� ����, ���
          [, ] �� ��ǯ, ��ǯ
        * C-u 20 - �� 20����
        * �ҥåȤ��ʤ��ä��餽��������դ���õ��
            (setq howm-date-forward-ymd-limit 90)  ;; 90����� give up
        * ��äȤޤ��ʥ�������ʤ����ͤ�
      * �������ϡ�C-c , d�פ����Ȥ���ư��򤵤�˾�������
          (setq howm-insert-date-pass-through t)
        * ���ե��ޥ�ɤˤĤ��Ƥϸ���Ʊ��
        * ���ե��ޥ�ɤ���ʤ��Ȥ���, ��������ȴ����.
          C-c , d hoge �Ȥ� C-c , d C-a �Ȥ���Ф狼��ޤ�.
        * ���ޤä�. ��[2004-05-21]+�פȤ����Ϥ��褦�Ȥ���ȤȤޤɤ�.
          ��+ RET�פǡ�+�������פˤϤ��Ƥߤ����ɡ�
    * ����¾
      * ���ޤ�
          (setq howm-congrats-command '("play" "~/sound/level.wav"))
  * 1.2.1
    * Major
      * ��˥塼�ˡ�%reminder�פȽ񤯤�, ͽ��� todo ���������
        * ͽ���@�פ�,
          howm-menu-schedule-days-before ��������
          howm-menu-schedule-days ����ޤǤ���Ƭ��ɽ��
          * [2004-12-03]@5 �ʤɤȽ񤯤�, ��5 ���֡פΰ�
            (������ޤ�Τǡ�12��3������12��7���ޤǡ�).
            ��������ä���Τ��������ͱͽ�����.
            ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/516>))
        * ���ڡ�!�פ�, �������������ϰϤޤǤʤ����ɽ��
        * �����겼�Ͻ���ɤ���
      * howm2 �κ��ľ��? (ext/howmkara)
        * ɬ�פˤ��ޤ��ƤǤä�����. ̾����Ƥ��Ȥ�.
          * ɬ�פ��������줿����, �ޤ����֤���. ï���ɤ��ˤ����Ƥ����С�
        * ��ǽ���ಽ. �������������Ϥޤ�.
          * magic string ������ФäƤ�ΤϤ�������󤱤ɡ�
        * �����ե������ʬ�䤹�� ext/hsplit.rb ��񤤤�����,
          ����Ϥ���˼�ȴ��
    * Minor
      * [2004-09-01 23:26]@ �Ȥ��� tag2plan ��ɽ��
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/398>))
        * ɽ�������äƤ���. �������ᤷ�ޤ���.
          ���ν񼰤��ܵ��ǥ��ݡ��Ȥ��뤫̤��ʤΤ�.
      * hcal.rb �Ρ�[2004-09-02]?���б�(��ʬ���Ѥ��ΤФ��Τ�)
        ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?TangledToDo>))
      * M-x howm-return-to-list �� ����ɽ�������
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/377>))
        * �Ȥ�ޤ�Ķ���Ǥä�����. ȿ���˱����Ƥޤ��ͤ��褦.
        * ����ɽ���ˤ���������뤳�Ȥʤ�, �����μ����ܤ�ľ�ܳ���:
            (defun my-howm-next-hit (n)
              (interactive "p")
              (let ((buf (save-window-excursion
                           (howm-return-to-list)
                           (when (not (eq major-mode 'howm-view-summary-mode))
                             (error "Sorry. This case is not cared."))
                           (forward-line n)
                           (let ((howm-view-summary-keep-cursor nil))
                             (howm-view-summary-open))
                           (current-buffer))))
                (switch-to-buffer buf)))
            (defun my-howm-previous-hit (n)
              (interactive "p")
              (my-howm-next-hit (- n)))
  * 1.2.2
    * ����
      * �Хåե�����: M-x howm-list-buffers
        * ��������ʤ���ߤʤɤ򤹤��, �����Хåե� occur�������Τ��Ȥ��Ǥ���
        * C-u ��Ĥ����, �����Хåե��ޤǤ��٤�
        * ��������Хåե�̾������
            (setq howm-list-buffers-exclude
                  '("*Messages*" ".howm-keys" ".howm-history"))
      * ���Хåե��κǶ�ޡ�������: M-x howm-list-mark-ring
    * �ü�ե����
      * namazu folder ���
        * �����ɻ�����
        * +from: �ʤɤ�̤�б�
        * ľ�ܸ�������ˤ� M-x howm-search-namazu
      * rot13 folder/page ���
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/580>))
        * ���켫�ΤϤ�ͷ�Ӥ�����, �֤դĤ��Ǥʤ��ڡ����פ������Ȥ���
        * rot13:xxx �Хåե���, C-c C-c �ǡ�rot13 ������¸��
          * rot13 �ʥե�����򳫤��ˤ�, M-x yarot13-find-file
      * howm-search-path ��, �̾�Ρ֥ǥ��쥯�ȥ�װʳ���񤱤�
          ;; namazu folder �� rot13 folder �򸡺��оݤ��ɲ�
          ;; (M-x howm-toggle-search-other-dir ��ͭ����̵�����ڤ꤫��)
          (let* ((nd "~/PATH/NMZ/Mail") ;; namazu ����ǥå����Τ���ǥ��쥯�ȥ�
                 (rd "~/g/r13") ;; ���Υǥ��쥯�ȥ�ʲ��Υե������ rot13 �����
                 (nf (howm-make-folder:namazu nd))
                 (rf (howm-make-folder:rot13dir rd)))
            (setq howm-search-path (list nf rf)))
          (howm-toggle-search-other-dir 1) ;; 0 �ʤ������֤ϡ�̵����
    * [2004-12-13]_3 ��ͱͽ������3�פΰ�̣�� 1 ���餷��
      * ���ޤޤǤ�, ��ά�� 0 �� 1 ��Ʊ����̣�ˤʤäƤ���
      * �����쵤����������, ��äȤޤ���˼������ʤ�������
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/522>))
    * ext/hcal.rb �� iCalendar ���Ϥ��ɲ�, �Ĥκ���¤ΤȤä��������
  * 1.3.0
    * ��ե�����ʣ�����ΤȤ���, �ʤ��������ե�����ñ�̤���ʤ����ñ�̤�.
      ������, date �Ǥιʤ���ߤϥե�����ñ�̤Τޤ�.
        (setq howm-view-search-in-result-correctly t)
    * ��˥塼�� %reminder ��˻��ڤ�
        (setq howm-menu-reminder-separators
              '(
                (-1  . "����������������������Ķ�ᨬ������������")
                (0   . "��������������ͽ�ꢭ��������������")
                (3   . "����������������ä��袭��3����ޤǨ�������������")
                (nil . "��������������todo����������������") ;ͽ���todo�ζ�
                ))
      * �Τξ��ٹ���[2005-05-17]_ ���������פ�, ���Τ����ѻ�
    * howm ��Ϣ�����Хåե��˶��̤ο�����
        ;; �֤ۤ��פȡ�[�դ�]�פ��忧
        ;; ������ˡ�ξܺ٤�, �ѿ� font-lock-keywords �Υإ�פ򻲾�
        ;; ��face �ΰ����� M-x list-faces-display
        (setq howm-user-font-lock-keywords
          '(
            ("�ۤ�" . (0 'highlight prepend))
            ("\\[�դ�\\]" . (0 'font-lock-doc-face prepend))
            ))
      * todo ��ͽ��ο��櫓�ˤǤ�ȤäƤϤ���������.
        ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/42>))
        ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?Idea>))
        > taku ����
    * ���������դȤ���, [2005-05-19] �Ǥʤ� 2005-05-19 ��ϥ��饤��
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/691>))
        (setq howm-highlight-date-regexp-format
              (regexp-quote howm-date-format))
      * ͽ��� [2005-05-19 20:54]@ �Τ褦�˽񤯿͸���
        * ��������äƤ���褦��,
          �֤��ν񼰤�ۤ�Ȥ˥��ݡ��Ȥ��뤫��̤��פǤ�.
          ��[2005-05-19]@ 20:54 �ġפ�����̵��.
          ((<ref|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?DateFormat>))
    * [2005-05-15 21:37]@ �Τ褦�ʽ񼰤�ͽ���, �����˥�����
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/683>))
        (setq howm-schedule-sort-by-time t)
      * �ƥ��Ȥ�����Ǥ� > 683 ����.
        �����ʾ���ʬ���ᤷ����� OK ��������Ƥ�����������,
        �ǥե���Ȥˤ��褦����.
    * ��ʸ���������ƥ� ((<Rast|URL:http://www.netlab.jp/rast/index.html.ja>))
      ��̣��
      * rast-0.1.1 �ǻ
      * �������ޥ�ɤȤ��� ext/howm-rast-search.rb �� ext/howm-rast-register.rb
        ��ɬ��
      * ����ɽ���θ����ˤ� rast ��Ȥ�ʤ� (���������٤��ʤ�)
        * ���Τ���, ��˥塼��ͽ��ɽ�� todo �ꥹ�Ȥ��٤�
      * �Ȥ���
        * rast �ǡ����١���������äƹ��ۤ��Ƥ���
        * �ɤ��餫����
          * howm-directory �� rast �Ǹ���������
              ;; rast �ǡ����١����ΰ��֤�, ��⤬�ºݤˤ���ǥ��쥯�ȥ�Ȥ����
              (setq howm-directory
                    (howm-make-folder:rast "/tmp/rastdb"
                                           (expand-file-name "~/howm/")))
              ;; ��˥塼�ե�������ۤ˻��ꤹ��ɬ�פ���
              (setq howm-menu-file
                    (expand-file-name "~/howm/0000-00-00-000000.howm"))
              ;; ��˾�ߤʤ�, �ե�������¸���˥ǡ����١�����ư����
              ;; (�������Ԥ����Ȥ��Τ��Ȥʤ󤫤Ϲͤ��Ƥʤ�)
              ;(setq howm-rast-register-command
              ;      (expand-file-name "~/elisp/howm/ext/howm-rast-register.rb"))
              ;(add-hook 'howm-after-save-hook
              ;  (lambda () (howm-rast-register-current-buffer "/tmp/rastdb")))
          * howm-directory �˲ä���, �̤Τɤ����� rast �Ǹ���������
              ;; rast �ǡ����١����ΰ���
              (setq howm-search-path
                (list (howm-make-folder:rast "/tmp/rastdb")))
              ;; �� = howm-search-path ��õ��
              (howm-toggle-search-other-dir 1)
        * ξ�Զ��̤ǡ�
            (setq howm-rast-search-command
                  (expand-file-name "~/elisp/howm/ext/howm-rast-search.rb"))
  * 1.3.1
    * �����ȥ뤬���ΤȤ�����ʸ�ΰ���ܤ򥿥��ȥ��. �Ť�������.
        ;; �����ȥ��󤬤���˥ޥå������Ȥ���, �ޥå����ʤ��ǽ�ιԤ�
        ;; ���إ����ȥ�Ȥ���
        (setq howm-view-title-skip-regexp
              "\\(^=? *$\\)\\|\\(^\\[[-: 0-9]+\\]\\)")
        ;; �������ꤷ�Ƥ����ʤ��Ȣ��ϵ�ǽ���ʤ�
        (setq howm-view-search-in-result-correctly t)
    * �������֥Х��λ�Ŧ�μ��װ�
      * make test �� emacs ��ư
      * �Х���ȯ�ɤ�����
        * ȯ�ɤ��ʤ����, ��ʬ�� .emacs �����Ϣ�������ʤȤ����
          sample/dot.emacs �إ��ԡ�����, �⤦���� make test
      * ȯ�ɤ����餹������ M-x howm-bug-shot
        * �С������䥹���꡼�󥷥�åȤʤɤ�ɽ������ޤ�
      * �����Ȥ�ä��� 2ch ��Ž��
  * 1.3.2
    * M-x howm-occur ��, �����ȥХåե��򸡺�
    * grep ���ѻ��� coding system ����
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/784>))
        (setq howm-process-coding-system 'euc-japan-unix)
  * 1.3.3
    * M-x howm-remember ��
      ((<remember-mode|URL:http://www.emacswiki.org/cgi-bin/emacs-en/RememberMode>))
      ��ɤ�
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/24-25n>))
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/61>))
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/72-75n>))
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/92-93n>))
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/99>))
      * �񤭹����ѥ�����ɥ����ݥåץ��åפ���Τǡ�
        * �٤��äȽ񤤤� C-c C-c �� ������¸��, �ݥåץ��åפ�ä�
        * ����󥻥뤹��ʤ� C-c C-k
      * �Ҥ�äȤ���, �֤ष���ä���ǥե���Ȥˤ����?
      * ����ܤ� howm-template �� %title ��, �Ĥ�� %cursor ��,
        �Ȥ��������
          (setq howm-remember-first-line-to-title t)
      * �����������򤹤٤� howm-remember �ˤ���ˤϡ�
          ;; howm-create �򤹤٤� howm-remember �ˤ��꤫����
          (defadvice howm-create (around remember activate)
            (if (interactive-p)
                (howm-remember)
              ad-do-it))
          (setcdr (assoc "[����]" howm-menu-command-table-ja)
                  '(howm-remember current))  ;; [2006-05-15] ����
        * ��˥塼��� c �򲡤����Ȥ�, �֥�˥塼������ɽ�����Ƥ����Хåե���
          ��Ф��������ߤʤ�, ��current�פ��previous�פ�ľ���Ƥ�������
    * ���ƥ����̤� todo list
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/885>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/890>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/909>))
      ((<thx|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/919>))
      * ��ʬ��פ���˾�ϤĤäѤͤƤ����������, �����ϵ��ޤ���˵����������Τ�
        ���. ������ǽ�ˤ��Ƥ������ɤ�����̤��.
      * ��˥塼�ˤ����񤯤�, ��foo�ס�bar�ס�baz�פ�ޤ� todo ��
        ʬ�ष��ɽ��
          %here%(howm-menu-categorized-reminder ("foo" "bar" "baz"))
        * ���ʤߤ�, %here% �Ǥϥ������Ȥ����פǤ�
      * �����, �ƹԤΡ�foo�ס�bar�ס�baz�פ�ä��������
          %here%(howm-menu-categorized-reminder ("foo" "bar" "baz") nil t)
      * ��misc.�פ���ɽ���ˤ��������
          %here%(howm-menu-categorized-reminder ("foo" "bar" "baz") nil nil t)
    * �����Хåե��Υޥå����Ƥκ��˥����ȥ��ɽ��.
      ���ʤߤ˽���Τ�, �֥ޥå����ƤΤ����˥����ȥ��ɽ����.
      ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2006/000025.html>)) > Highfly ����
        (setq howm-view-list-title-type 2) ;; �ޥå����Ƥκ��˥����ȥ��ɽ��
        (setq howm-view-summary-format "") ;; �ե�����̾��ä��������
    * C-c , M �ǡ֥ե�����̾����ꤷ�ƥ��򳫤���
      ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2005/000010.html>)) > Eduardo Ochs ����
    * %reminder ��λ��ڤ�ˤĤ���
      * 1.2.1 ����α�����ǽ���ä������ѻ�
        * ���Τɤ����˲��Τ褦�˽񤤤Ƥ�����, �����ΰ��֤ˤ��줬ɽ�������
            [2004-11-01]_0 ��������������������������
          * ���դϥ��ߡ�. 0 �ϡֺ��� - 0 ���פΰ���.
          * (��˥塼����ʤ�) todo �����ˤ�ФƤ��ޤ���������
      * �������ѿ� howm-menu-reminder-separators ��ȤäƤ�������
      * 1.3.0 �α�����ǽ��������Ƥ����Ȥ���
  * 1.3.7
    * ��˥塼�����ޥ������ľá�������Ȥ���,
      �б��Хåե��ιԿ���¿������Ƥ��Ƥ����.
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/519>))
        (setq howm-action-lock-forward-fuzziness 5) ;; ���ԤޤǤ���Ƥ������
    * �������˥�����ɥ�ʬ�������ʤ�����.
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/507>))
        (setq riffle-keep-window t)
        (setq riffle-window-initializer nil)
      * ���ƥХåե���ɽ������ޤ���.
      * �Ȥꤢ����á����. �����ѹ��β�ǽ������.
    * M-x howm-list-active-todo ��, ����ͭ����(��������Ǥʤ�) todo �Τߤ����.
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/129-131n>))
      * �Ĥ��Ǥ�, M-x howm-list-sleeping-todo ��, ������� todo �Τߤ����.
      * �Ȥꤢ�����ؿ��������. ���󥿥ե������ΰƤ�����Ф�ʹ������������.
      * ���ʤߤ�, ��˥塼���������� todo ��ä��ˤ�,
        M-x customize-variable RET howm-menu-todo-priority RET ��
        ��Hide sleeping reminders�פ����ꤷ�Ƥ�������.
    * todo ����(M-x howm-list-todo)�ˤ���ڤ��ɽ��
        (setq howm-todo-separators
              '(
                (0  . "����������������Ķ�ᨬ������������")
                (nil . "�������������������梭��������������")
                ))
      * M-x howm-list-active-todo �ؤ��аƤȤ��ƻ��.
      * Ϣ��ɽ���䥽���Ȥ򤹤���ˤϤ���ޤ����
    * �Хåե�̾��, �ե�����̾�ǤϤʤ������ȥ�ˤ���.
      ((<thx|URL:http://lists.sourceforge.jp/mailman/archives/howm-eng/2006/000020.html>)) > Mielke-san (peter at exegenix.com),
      ((<thx|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?ExternalTool>))
        ;; emacs ��ǤΥХåե�̾��, �ե�����̾�ǤϤʤ������ȥ��.
        ;; (�ե�����̾���Τ��ѹ����ʤ�)
        (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
        (add-hook 'after-save-hook 'howm-mode-set-buffer-name)
      * �����ȥ�֤ۤ��פΥ��ΥХåե�̾���=�ۤ��פ�
        ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/333>))
          ;; �����ȥ뤬 AAA �ʤ�Хåե�̾�� =AAA ��.
          ;; ���� howm-mode-set-buffer-name �����ꤷ����ǡ�
          (setq howm-buffer-name-format "=%s")
      * ������ howm ����Ω�����ġ���ˤ���������������,
        ���ä����ʤΤǤҤȤޤ�.
    * howm-occur �� grep ����
        ;; M-x howm-occur ���ϳ������ޥ�ɤ� grep ��Ȥ�ʤ��褦����.
        ;; �� grep �Ǥʤ� emacs ������ɽ�������
        ;; �� ����ɽ��������ϥ��饤�Ȥ����
        ;; �� ��ü�˵���ʥХåե������٤�����
        (setq howm-occur-force-fake-grep t)
  * 1.3.8
    * M-x howm-extend-deadlines ��,
      ��������᤮������(!)�򤹤٤ư���������ر��.
      ((<thx|URL:http://pc11.2ch.net/test/read.cgi/unix/1141892764/671>))
      * ���ͤ⥤�󥿥ե�������ޤ�á����.
      * ���򾡼�˽񤭤�����������ʤΤ�, �Хå����åפ�ȤäƤ���!
  * 1.3.9
    * ��˥塼�ˡ�%here%(howm-menu-search "[��]��Ū" full t)�פȽ񤱤�,
      ������Ū�פ�ޤ�����������Τ��˥塼��������.
      ((<thx|URL:http://sourceforge.jp/projects/howm/lists/archive/eng/2010/000097.html>))
      > Morgan Veyret ���� (morgan.veyret at gmail.com).
      * ñ�� "����Ū" �Ȥ��ʤ��Τ�, ��˥塼�ե����뼫�Ȥ��ҥåȤ���Τ��򤱤뾮�ٹ�

* ������ȴ��� (((<URL:OLD.rd>)) ����)
  * [2005-08-02] 1.3.0 alias. M-x customize. �����ȥ�ɽ��.
  * [2005-05-02] 1.2.2 �Хå�������ڤ�Υ��. gfunc.el
  * [2004-08-24] 1.2 ��α��~�פθ�����. howm.el, riffle.el
  * [2004-05-06] 1.1.2 make test
  * [2004-02-27] ((<"2ch howm ���� 2"|URL:http://pc8.2ch.net/test/read.cgi/unix/1077881095/>))
  * [2004-02-21] 1.1.1 �ֱ�����ǽ�����٤�Ƴ��
  * [2004-01-25] ((<"sf.jp"|URL:http://howm.sourceforge.jp/>)) �ذ�ư
  * [2005-01-08] ((<"UNIX USER 2004.2"|URL:http://www.unixuser.jp/magazine/2004/200402.html>))
  * [2003-12-27] ((<howm wiki|URL:http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi>))
  * [2003-11-22] 1.1 �ǥե���Ȥ��ѹ�
    (��󥯡����ա���ޥ�����ν�, �����ե�����, ��˥塼����ΰ��)
  * [2003-10-27] 1.0.4.2 �Ť��ʤ�Х�����. �褯����ʤΤ�ư���Ƥ��ʤ���
  * [2003-10-02] 1.0.4 ���� viewer, ��˥塼�β�궯��
  * [2003-09-23] �֥ƥ����ǡפ�Ƴ��
  * [2003-09-18] 1.0.2 HTML ��������ץ� howm2
  * [2003-09-17] ((<2ch howm ����|URL:http://pc.2ch.net/test/read.cgi/unix/1063800495/>))
  * [2003-09-17] 1.0 ���ǥ��쥯�ȥ���ز�
  * [2003-09-16] 0.9.7.1 Wiki ����� [[�ۤ�]]
  * [2003-09-14] 0.9.4.1 grep æ��
  * [2003-09-09] 0.9 ruby æ��
  * [2003-08-31] 0.8.5 �����ȥ����
  * [2003-06-03] 0.8.4 ��ľ�������� hcal.rb
  * [2002-11-03] 0.8 ��˥塼, ���ٽ� todo @[2003/09/20]+
  * [2002-09-17] 0.7 1 �� 1 �ե�����, come-from ��� <<
  * [2002-09-14] 0.6 ����ѻ�(���٤Ƥϡָ�����)
  * [2002-06-10] ((<"����ȯ�� wiki ������ꥹ��"|URL:http://www1.neweb.ne.jp/wa/yamdas/column/technique/clonelist.html>))
  * [2002-05-29] 0.1 ����

=== ���ɥ쥹

* �ǿ���: ((<URL:http://howm.sourceforge.jp/>))
* Ϣ����: email ���ɥ쥹�ϥ������ե�������Ƭ�򻲾Ȥ�������

=end

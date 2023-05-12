package Solvers.Resolution;

/** characterises the distribution of positive and negative literals in a clause. */
public enum ClauseType {
    /** clause consists of positive literals only. */
    POSITIVE,

    /** clause consists of negative literals only. */
    NEGATIVE,

    /** clause consists of positive and negative literals.
     * Neither the positive nor the negative literals are sufficient to satisfy the atleast-limit.*/
    MIXEDMIXED,

    /** clause consists of positive and negative literals.
     * The positive literals are sufficient to satisfy the atleast-limit.*/
    MIXEDPOSITIVE,

    /** clause consists of positive and negative literals.
     * The negative literals are sufficient to satisfy the atleast-limit.*/
    MIXEDNEGATIVE,

    /** clause consists of positive and negative literals.
     * Both, the positive and the negative literals are sufficient to satisfy the atleast-limit.*/
    POSITIVENEGATIVE
}

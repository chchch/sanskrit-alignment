import {describe, it} from 'mocha';
import {expect} from "chai";
import {Sanscript} from "../lib/sanscript.js";

describe('sanscript', () => {
  let sanscript = null
  beforeEach(() => {
    sanscript = new Sanscript()
    // TODO: use something more meaningful?
    sanscript.romanSchemes['test'] = true
  })
  it('isRomanScheme', () => {
    const tests = [
      {
        given: 'sardines',
        expected: false
      },
      {
        given: 'test',
        expected: true
      }
    ]
    for (const test of tests) {
      expect(sanscript.isRomanScheme(test.given)).to.equal(test.expected)
    }
  })
})
